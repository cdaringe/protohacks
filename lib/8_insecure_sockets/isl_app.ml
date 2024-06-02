(* open Eio
open Lrcp_session
module IntMap = CCMap.Make (Int)
module S = SessionManager

let create_handler ~clock ~sw =
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
      let handler = create_handler ~clock ~sw in
      Server.listen_udp ~swo:(Some sw) ~env ~port ~fn:handler ()) *)
