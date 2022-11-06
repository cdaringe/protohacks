open Eio
open Eio.Net
module Read = Eio.Buf_read
module Write = Eio.Buf_write

let exn_string e =
  let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
  Fmt.str "%s%s" msg stack

let listen_ ~sw ~env ~port ~fn =
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Ipaddr.V4.any, port) in
  let socket = listen ~reuse_addr:true ~backlog:10 ~sw net addr in
  let on_error e =
    traceln "[cserver] error handling connection: %s" (exn_string e)
  in
  let safe_fn a b =
    try fn a b
    with e -> (
      traceln "[cserver] socket handler raised with: %s\nattempting shutdown"
        (exn_string e);
      try Flow.shutdown a `All
      with e -> traceln "[cserver] shutdown attempted: %s" (exn_string e))
  in
  traceln "server listening on %i" port;
  while true do
    accept_fork socket ~sw ~on_error safe_fn
  done

let listen ?(swo = None) ~env ~port ~fn () =
  let go ~sw = listen_ ~sw ~env ~port ~fn in
  match swo with None -> Switch.run @@ fun sw -> go ~sw | Some sw -> go ~sw

let listen_udp_ ~sw ~env ~port ~fn =
  let net = Eio.Stdenv.net env in
  let addr = `Udp (Ipaddr.V4.any, port) in
  let socket = datagram_socket ~sw net addr in
  let on_error e =
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    traceln "error handling connection: %s%s" msg stack
  in
  let reply client_addr msg = send socket client_addr (Cstruct.of_string msg) in
  traceln "server listening on %i" port;
  while true do
    let buf = Cstruct.create 1000 in
    try
      let client_addr, r = recv socket buf in
      let msg = Cstruct.(to_string (sub buf 0 r)) in
      let len = msg |> String.length in
      if len < 1000 then fn ~msg ~addr:client_addr ~reply:(reply client_addr)
      else failwith (Printf.sprintf "msg too long %i" len)
    with e -> on_error e
  done

let listen_udp ?(swo = None) ~env ~port ~fn () =
  let go ~sw = listen_udp_ ~sw ~env ~port ~fn in
  match swo with None -> Switch.run @@ fun sw -> go ~sw | Some sw -> go ~sw

let nl = '\n'

let read_line buf =
  let line = Read.take_while (fun c -> c != nl) buf in
  Buf_read.char nl buf |> ignore;
  line

let read_lines_exn ~buf ~on_line =
  while true do
    on_line @@ read_line buf;
    Fiber.yield ()
  done

let read_bytes ~flow num_bytes =
  Read.of_flow flow ~initial_size:num_bytes ~max_size:num_bytes

let read_line_term_char ?(term = '\n') flow =
  let buf = Read.of_flow flow ~initial_size:100 ~max_size:1_000_000 in
  Read.take_while (fun c -> c != term) buf

let read_lines ~flow ~on_line =
  let buf = Read.of_flow flow ~initial_size:100 ~max_size:1_000_000 in
  try read_lines_exn ~buf ~on_line with End_of_file -> ()

let send_bytes ~flow ?(flush = true) bytes =
  let write_flush t =
    if flush then Write.flush t;
    Write.bytes t bytes;
    if flush then Write.flush t
  in
  Write.with_flow flow write_flush

let send_line ~flow text =
  let line = text ^ "\n" in
  let write_flush t = Write.string t line in
  Write.with_flow flow write_flush
(* Eio.traceln "(sent line: %s)" (String.trim line) *)

let send_ndjson ~tojson ~flow t = tojson t |> send_line ~flow
