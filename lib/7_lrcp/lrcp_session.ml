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
    mutable max_in_ack_len : int;
    mutable last_data_ack : float;
    mutable in_watermark : int;
    mutable out_watermark : int;
    mutable history : data_tuple list;
    mutable is_closing : bool;
    mutable outbound_queue : string list;
    mutable pending_ack : data_tuple option;
    client : Client.t;
  }

  type session_event =
    | Open
    | Data of { data : string; active_client : active_client }
    | Close

  type manager = {
    mutable clients_by_id : active_client IntMap.t;
    clock : Time.clock;
    on_event : session_event -> unit;
    sw : Switch.t;
  }

  type t = manager ref

  let init ~on_event ~clock ~sw =
    ref { sw; clock; clients_by_id = IntMap.empty; on_event }

  let incr_out_watermark ac data =
    let len = String.length data in
    let next_watermark = ac.out_watermark + len in
    (* traceln "out_watermark (%i) + len (%i) = %i" ac.out_watermark len
       next_watermark; *)
    ac.out_watermark <- next_watermark

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
                (* traceln "retry heartbeat %f %f" prev_last_data_ack ac.last_data_ack; *)
                Eio.Time.sleep !t.clock 3.;
                let next_ac_opt = get_active_client_opt ac.client.id t in
                continue :=
                  match (ac.is_closing, next_ac_opt) with
                  | true, _ | _, None ->
                      (* don't try resending, we're shutting down or have a new client *)
                      false
                  | _, Some next_ac ->
                      if next_ac.last_data_ack = prev_last_data_ack then (
                        traceln "[c%i] retrying, unacked %s" next_ac.client.id
                          msg_str;
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
    (* always use a new client for close messages, as clients may have
       already been purged. clients are cheap. *)
    let client = create_client ~id ~addr ~reply in
    send ~client id (Close id) t

  let close_with_client ({ id; addr; reply; _ } : Client.t) t =
    close ~id ~addr ~reply t

  let send_data_after ~after ac t =
    let is_msg_after (_, pos, _) = pos > after in
    let resend raw_msg = send ac.client.id (Data raw_msg) t in
    let as_ascending = List.rev in
    List.filter is_msg_after ac.history |> as_ascending |> List.iter resend

  (* internal *)

  let monitor_inactivity client_id t =
    let rec fn _ =
      Eio.Time.sleep !t.clock 60.;
      match IntMap.find_opt client_id !t.clients_by_id with
      | Some active_client ->
          let now = Time.now !t.clock in
          if now -. active_client.last_data_ack > 60. then (
            traceln "[c%i] inactive" client_id;
            close_with_client active_client.client t;
            `Stop_daemon)
          else fn ()
      | _ -> `Stop_daemon
    in
    Fiber.fork_daemon ~sw:!t.sw fn

  (* inbound *)
  let add (client : Client.t) t =
    match is_active_client client.id t with
    | true -> traceln "[sm] c%i already active" client.id
    | false ->
        monitor_inactivity client.id t;
        let next_clients =
          IntMap.add client.id
            {
              client;
              last_data_ack = Time.now !t.clock;
              max_in_ack_len = 0;
              in_watermark = 0;
              out_watermark = 0;
              history = [];
              is_closing = false;
              outbound_queue = [];
              pending_ack = None;
            }
            !t.clients_by_id
        in
        !t.clients_by_id <- next_clients

  let incr_in_watermark ac data =
    ac.in_watermark <- ac.in_watermark + String.length data

  let try_consume_queue ac t =
    match ac.pending_ack with
    | Some _ -> ()
    | None -> (
        match ac.outbound_queue with
        | [] -> ()
        | data :: datas ->
            let payload = (ac.client.id, ac.out_watermark, data) in
            incr_out_watermark ac data;
            let msg = Lrcp.Data payload in
            (* if we're sending data, update our pending ack state *)
            ac.history <- payload :: ac.history;
            ac.pending_ack <- Some payload;
            ac.outbound_queue <- datas;
            send ac.client.id msg t)

  let enqueue_data_string ac data t =
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
    aux data;
    try_consume_queue ac t

  module In = struct
    let on_data active_client pos data t =
      (* traceln "in_watermark (pre): %i, pos: %i" active_client.in_watermark pos; *)
      if active_client.in_watermark = pos then (
        (* expected new data. grow the good data watermark and process *)
        incr_in_watermark active_client data;
        (* traceln "in_watermark (post): %i, pos: %i" active_client.in_watermark pos; *)
        ack ~pos:active_client.in_watermark active_client.client.id t;
        let evt = Data { active_client; data } in
        !t.on_event evt)
      else ack ~pos:active_client.in_watermark active_client.client.id t

    let on_connect id addr reply t =
      let client = create_client ~id ~addr ~reply in
      add client t;
      ack id t;
      ()

    let on_ack ~active_client ~len t =
      let ac = active_client in
      active_client.last_data_ack <- Time.now !t.clock;
      let id = ac.client.id in
      if len < ac.max_in_ack_len then
        traceln "[c%i] ack len < previous max ack len (%i -> %i)" id len
          ac.max_in_ack_len
      else if len > ac.out_watermark then (
        traceln "[c%i] misbehavin len %i > out_watermark %i" id len
          ac.out_watermark;
        close_with_client active_client.client t)
      else if len < ac.out_watermark then (
        traceln "[c%i] (acked@%i, watermark@%i) retransmitting" id len
          ac.out_watermark;
        send_data_after ~after:len ac t)
      else (
        (* we got ack'd, unblock the queue! *)
        ac.pending_ack <- None;
        traceln "[c%i] good ack" id;
        try_consume_queue ac t)

    let create_handler ~msg:raw_msg ~addr ~reply (t : t) =
      traceln "> %s" (escape_newlines raw_msg);
      let with_ac_or_close id f =
        match get_active_client_opt id t with
        | Some ac -> f ac
        | None -> close ~id ~addr ~reply t
      in
      try
        match In.parse raw_msg with
        | Connect id -> on_connect id addr reply t
        | Data (id, pos, data) ->
            with_ac_or_close id (fun ac -> on_data ac pos data t)
        | Ack (id, len) ->
            with_ac_or_close id (fun ac -> on_ack ~active_client:ac ~len t)
        | Close id ->
            with_ac_or_close id (fun ac -> close_with_client ac.client t)
      with Lrcp.In.Parse_msg e -> traceln "%s" e
  end
end
