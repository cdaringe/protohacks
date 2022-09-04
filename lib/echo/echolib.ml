open Eio
open Eio.Net

let listen ~env ~port =
  Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Ipaddr.V4.any, port) in
  let socket = listen ~sw ~backlog:1000 net addr in
  let echo flow _ =
    traceln "starting echo";
    Flow.copy flow flow
  in
  let on_error = traceln "Error handling connection: %a" Fmt.exn in
  traceln "server started";
  while true do
    accept_fork socket ~sw echo ~on_error
  done
