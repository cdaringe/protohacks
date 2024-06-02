open Eio
open Lrcp
module IntMap = Map.Make (Int)

let escape_newlines = CCString.replace ~sub:"\n" ~by:"\\n"

module Client = struct
  type t = { id : int; addr : Net.Sockaddr.datagram; reply : string -> unit }

  let create ~id ~addr ~reply =
    let t = { id; addr; reply } in
    t
end

module SessionManager = struct
  exception Client_inactive

  type active_client = {
    client : Client.t;
    mutable history : data_tuple list;
    mutable in_watermark : int;
    mutable is_closing : bool;
    mutable last_data_ack : float;
    mutable last_data_ts : float;
    mutable out_watermark : int;
    mutable outbound_queue : string list;
  }

  type session_event =
    | Open
    | Data of { data : string; active_client : active_client }
    | Close

  type manager = {
    mutable clients_by_id : active_client IntMap.t;
    clock: (float Time.clock_ty) Resource.t;
    on_event : session_event -> unit;
    sw : Switch.t;
  }

  type t = manager ref

  let init ~on_event ~clock ~sw =
    ref { sw; clock; clients_by_id = IntMap.empty; on_event }

  let get_active_client_opt id t = IntMap.find_opt id !t.clients_by_id

  let get_active_client_then id t fn =
    match get_active_client_opt id t with
    | Some active_client -> fn active_client
    | _ -> traceln "[sm] c%i not found" id

  let is_active_client id t = IntMap.mem id !t.clients_by_id

  let with_active_client id t f =
    match IntMap.find_opt id !t.clients_by_id with None -> () | Some x -> f x

  let remove_client id t =
    t := { !t with clients_by_id = IntMap.remove id !t.clients_by_id }

  let send ?client id (m : Lrcp.t) (t : t) =
    let msg_str = Out.of_t m in
    let client', aco =
      match (client, get_active_client_opt id t) with
      | Some c, _ -> (Some c, None)
      | None, Some ac -> (Some ac.client, Some ac)
      | _ -> (None, None)
    in
    match client' with
    | Some { reply; _ } ->
        let send_msg _ =
          traceln "< %s" (escape_newlines msg_str);
          reply msg_str
        in
        let autoretransmit ac =
          let aux _ =
            try
              let prev_last_data_ack = ac.last_data_ack in
              let continue = ref (match m with Data _ -> true | _ -> false) in
              while !continue do
                Eio.Time.sleep !t.clock 3.;
                let next_ac_opt = get_active_client_opt ac.client.id t in
                continue :=
                  match (ac.is_closing, next_ac_opt) with
                  | true, _ | _, None -> false
                  | _, Some next_ac ->
                      if next_ac.last_data_ack = prev_last_data_ack then (
                        send_msg ();
                        true)
                      else false
              done
            with _ -> ()
          in
          Fiber.fork ~sw:!t.sw aux
        in
        Option.iter autoretransmit aco;
        send_msg ()
    | None -> ()

  (* outbound *)
  let create_client ~id ~addr ~reply = Client.create ~id ~addr ~reply
  let ack ?(pos = 0) id t = send id (Ack (id, pos)) t

  let close ~id ~addr ~reply t =
    get_active_client_then id t (fun ac ->
        ac.is_closing <- true;
        ac.outbound_queue <- [];
        remove_client id t);
    send ~client:(create_client ~id ~addr ~reply) id (Close id) t

  let close_with_client ({ id; addr; reply; _ } : Client.t) t =
    close ~id ~addr ~reply t

  let send_data_from ~aftereq ac t =
    let is_msg_aftereq (_, pos, _) = pos >= aftereq in
    let resend raw_msg = send ac.client.id (Data raw_msg) t in
    let as_ascending = List.rev in
    List.filter is_msg_aftereq ac.history |> as_ascending |> List.iter resend

  let monitor_inactivity client_id t =
    let rec fn _ =
      Eio.Time.sleep !t.clock 60.;
      match IntMap.find_opt client_id !t.clients_by_id with
      | Some active_client ->
          let now = Time.now !t.clock in
          let is_timed_out = now -. active_client.last_data_ack > 60. in
          if is_timed_out then (
            close_with_client active_client.client t;
            `Stop_daemon)
          else fn ()
      | _ -> `Stop_daemon
    in
    Fiber.fork_daemon ~sw:!t.sw fn

  let rec drain_queue ac t =
    match (ac.outbound_queue, ac.is_closing) with
    | _, true -> `Stop_daemon
    | [], _ ->
        Eio.Time.sleep !t.clock 0.01;
        drain_queue ac t
    | data :: datas, _ ->
        ac.outbound_queue <- datas;
        ac.out_watermark <- ac.out_watermark + String.length data;
        let payload = (ac.client.id, ac.out_watermark, data) in
        let msg = Lrcp.Data payload in
        ac.history <- payload :: ac.history;
        send ac.client.id msg t;
        drain_queue ac t

  let add (client : Client.t) t =
    match is_active_client client.id t with
    | true -> traceln "[sm] c%i already active" client.id
    | false ->
        monitor_inactivity client.id t;
        let ac =
          {
            client;
            last_data_ts = 0.;
            last_data_ack = Time.now !t.clock;
            in_watermark = 0;
            out_watermark = 0;
            history = [];
            is_closing = false;
            outbound_queue = [];
          }
        in
        let next_clients = IntMap.add client.id ac !t.clients_by_id in
        Fiber.fork_daemon ~sw:!t.sw (fun _ -> drain_queue ac t);
        !t.clients_by_id <- next_clients

  let enqueue_data_string ac data =
    let append_q x =
      ac.outbound_queue <- List.concat [ ac.outbound_queue; [ x ] ]
    in
    let rec aux s =
      match CCString.take_drop 950 s with
      | "", _ -> ()
      | x, y ->
          append_q x;
          aux y
    in
    aux data

  module In = struct
    let on_data active_client pos data t =
      let ac = active_client in
      ac.last_data_ts <- Time.now !t.clock;
      let id = ac.client.id in
      if ac.in_watermark = pos then (
        ac.in_watermark <- ac.in_watermark + String.length data;
        ack ~pos:ac.in_watermark id t;
        let evt = Data { active_client; data } in
        !t.on_event evt)
      else
        (* client probably dropped our last ACK. re-send the ACK it iff we have genuniely fizzled out on receiving data messages *)
        Fiber.fork ~sw:!t.sw (fun _ ->
            let prev = ac.last_data_ts in
            Eio.Time.sleep !t.clock 1.;
            if prev = ac.last_data_ts then ack ~pos:ac.in_watermark id t)

    let on_connect id addr reply t =
      add (create_client ~id ~addr ~reply) t;
      ack id t

    let on_ack ~active_client ~len t =
      let ac = active_client in
      ac.last_data_ack <- Time.now !t.clock;
      if len > ac.out_watermark then close_with_client ac.client t
      else if len < ac.out_watermark then
        (* allot a cooldown period before retransmitting. allow latent acks to cancel *)
        Fiber.fork ~sw:!t.sw (fun _ ->
            let prev = ac.last_data_ack in
            Eio.Time.sleep !t.clock 1.;
            if prev = ac.last_data_ack then send_data_from ~aftereq:len ac t)
      else ()

    let create_handler ~msg:raw_msg ~addr ~reply (t : t) =
      traceln "> %s" (escape_newlines raw_msg);
      let ac_or_close id f =
        match get_active_client_opt id t with
        | Some ac -> f ac
        | None -> close ~id ~addr ~reply t
      in
      try
        match In.parse raw_msg with
        | Connect id -> on_connect id addr reply t
        | Data (id, pos, data) ->
            ac_or_close id (fun ac -> on_data ac pos data t)
        | Ack (id, len) ->
            ac_or_close id (fun ac -> on_ack ~active_client:ac ~len t)
        | Close id -> ac_or_close id (fun ac -> close_with_client ac.client t)
      with Lrcp.In.Parse_msg e -> traceln "%s" e
  end
end
