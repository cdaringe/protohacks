open Eio
open Lrcp_session
module IntMap = CCMap.Make (Int)
module S = SessionManager

let send_reverse ~dbc ~(active_client : S.active_client) data =
  let id = active_client.client.id in
  let state = IntMap.get_or id !dbc ~default:"" ^ data in
  let final_chunk =
    if String.ends_with ~suffix:"\n" state then `Send else `Store
  in
  let filter_empty = List.filter (fun x -> x <> "") in
  let parts = String.split_on_char '\n' state |> filter_empty in
  let emit line_to_rev =
    let msg = CCString.rev line_to_rev ^ "\n" in
    S.enqueue_data_string active_client msg
  in
  let update_db v = dbc := IntMap.add id v !dbc in
  let rec process_line_parts = function
    | [] -> ()
    | x :: [] -> (
        match final_chunk with
        | `Send ->
            emit x;
            update_db "";
            ()
        | `Store -> update_db x)
    | x :: xs ->
        emit x;
        process_line_parts xs
  in
  process_line_parts parts

let create_lrcp_handler ~clock ~sw =
  (* dbc: data by clientid *)
  let dbc = ref IntMap.empty in
  let on_event = function
    | S.Data { active_client; data } -> send_reverse ~dbc ~active_client data
    | _ -> ()
  in
  S.(In.create_handler (init ~on_event ~sw ~clock))

let listen ~env ~port =
  let clock = Eio.Stdenv.clock env in
  Switch.run (fun sw ->
      let handler = create_lrcp_handler ~clock ~sw in
      Server.listen_udp ~swo:(Some sw) ~env ~port ~fn:handler ())
