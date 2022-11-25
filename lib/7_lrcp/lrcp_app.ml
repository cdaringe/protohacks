open Eio
open Lrcp_session
module IntMap = CCMap.Make (Int)
module S = SessionManager

let send_reverse_lines ~dbc ~(active_client : S.active_client) data =
  let id = active_client.client.id in
  let state = IntMap.get_or id !dbc ~default:"" ^ data in
  let final_chunk =
    if String.ends_with ~suffix:"\n" state then `Send else `Store
  in
  let parts =
    String.split_on_char '\n' state |> List.filter (fun x -> x <> "")
  in
  (* traceln "processing (%i lines) (final: %s)" (List.length parts)
     (if final_chunk = `Store then "STORE" else "SEND"); *)
  let emit line_to_rev =
    let msg = CCString.rev line_to_rev ^ "\n" in
    S.enqueue_data_string active_client msg
  in
  let update_db v = dbc := IntMap.add id v !dbc in
  let rec process = function
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
        process xs
  in
  process parts

let create_lrcp_handler ~clock ~sw =
  (* data by clientid *)
  let dbc = ref IntMap.empty in
  let smo = ref None in
  let on_event = function
    | S.Open -> ()
    | S.Data { active_client; data } ->
        send_reverse_lines ~dbc ~active_client data
    | S.Close -> ()
  in
  let sessionm = S.init ~on_event ~sw ~clock in
  smo := Some sessionm;
  S.In.create_handler sessionm

let listen ~env ~port =
  let clock = Eio.Stdenv.clock env in
  Switch.run (fun sw ->
      let handler = create_lrcp_handler ~clock ~sw in
      Server.listen_udp ~swo:(Some sw) ~env ~port ~fn:handler ())
