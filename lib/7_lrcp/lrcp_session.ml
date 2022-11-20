open Eio
open Lrcp
module IntMap = Map.Make (Int)

module Client = struct
  type t = { id : int; addr : Net.Sockaddr.datagram; reply : string -> unit }

  let create ~id ~addr ~reply =
    let t = { id; addr; reply } in
    t
end

module SessionManager = struct
  exception Client_inactive

  type active_client = {
    mutable max_msg_len : int;
    mutable total_bytes : int;
    mutable last_seen : float;
    mutable watermark : int;
    mutable history : Lrcp.lrcp_data_msg list;
    client : Client.t;
  }

  type session_event =
    | Open
    | Data of { session_id : int; data : string }
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

  let send id m t =
    get_active_client_then id t (fun ac ->
        let _ =
          match m with
          | `Data (_, __, data) ->
              ac.max_msg_len <- Int.max (String.length data) ac.max_msg_len
          | _ -> ()
        in
        let msg_str = Out.of_t m in
        traceln "< %s" msg_str;
        ac.client.reply msg_str)

  (* outbound *)
  let ack ?(pos = 0) id t = send id (`Ack (id, pos)) t

  let close id t =
    send id (`Close id) t;
    remove_client id t;
    ()

  let send_data_after ~after ac t =
    let is_msg_after (`Data (_, pos, _)) = pos > after in
    let resend m = send ac.client.id m t in
    List.filter is_msg_after ac.history |> List.iter resend

  (* internal *)

  let monitor_inactivity client_id t =
    let rec fn _ =
      Eio.Time.sleep !t.clock 60.;
      match IntMap.find_opt client_id !t.clients_by_id with
      | Some active_client ->
          let now = Time.now !t.clock in
          if now -. active_client.last_seen > 60. then (
            traceln "[c%i] inactive" client_id;
            close client_id t;
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
              last_seen = Time.now !t.clock;
              max_msg_len = 0;
              watermark = 0;
              total_bytes = 0;
              history = [];
            }
            !t.clients_by_id
        in
        traceln "[sm] adding client c%i" client.id;
        !t.clients_by_id <- next_clients

  let incr_watermark ac data = ac.watermark <- ac.watermark + String.length data

  let on_data active_client pos data t =
    (* traceln "watermark (pre): %i, pos: %i" active_client.watermark pos; *)
    if active_client.watermark = pos then (
      incr_watermark active_client data;
      (* traceln "watermark (post): %i, pos: %i" active_client.watermark pos; *)
      ack ~pos:active_client.watermark active_client.client.id t;
      let evt = Data { session_id = active_client.client.id; data } in
      !t.on_event evt)
    else ack ~pos:active_client.watermark active_client.client.id t

  let on_connect id addr reply t =
    let client = Client.create ~id ~addr ~reply in
    add client t;
    ack id t;
    ()

  let on_ack ~active_client:ac ~len t =
    match (len > ac.total_bytes, len < ac.max_msg_len) with
    | true, _ -> close ac.client.id t
    | _, true ->
        let is_unacked_msg (`Data (_, pos, d)) = pos + String.length d > len in
        ac.history <- List.filter is_unacked_msg ac.history
    | _ -> send_data_after ~after:len ac t

  let handle_msg ~msg:raw_msg ~addr ~reply (t : t) =
    traceln "> %s" raw_msg;
    let with_ac id f =
      match get_active_client_opt id t with
      | Some ac -> f ac
      | None -> close id t
    in
    match In.parse raw_msg with
    | `Connect sess_id -> on_connect sess_id addr reply t
    | `Data (id, pos, data) -> with_ac id (fun ac -> on_data ac pos data t)
    | `Ack (id, len) -> with_ac id (fun ac -> on_ack ~active_client:ac ~len t)
    | `Close id -> close id t
end
