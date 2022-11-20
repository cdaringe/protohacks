open Eio
open Lrcp_session

let listen ~env ~port =
  let clock = Eio.Stdenv.clock env in
  Switch.run (fun sw ->
      let module S = SessionManager in
      let on_event = function S.Open -> () | S.Data _ -> () | S.Close -> () in
      let sessionm = S.init ~on_event ~sw ~clock in
      let handler = S.handle_msg sessionm in
      Server.listen_udp ~swo:(Some sw) ~env ~port ~fn:handler)
