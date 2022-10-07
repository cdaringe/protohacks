open Eio
open Eio.Net
module Read = Eio.Buf_read
module Write = Eio.Buf_write

let listen_ ~sw ~env ~port ~fn =
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Ipaddr.V4.any, port) in
  let socket = listen ~sw ~backlog:1000 net addr in
  let on_error e =
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    traceln "error handling connection: %s%s" msg stack
  in
  traceln "server listening on %i" port;
  while true do
    accept_fork socket ~sw ~on_error fn
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
      traceln "ack";
      let raw_msg = Cstruct.(to_string (sub buf 0 r)) in
      let msg = raw_msg |> String.trim in
      let len = raw_msg |> String.length in
      if len < 1000 then fn ~msg ~addr:client_addr ~reply:(reply client_addr)
      else failwith (Printf.sprintf "msg too long %i" len)
    with e -> on_error e
  done

let listen_udp ?(swo = None) ~env ~port ~fn () =
  let go ~sw = listen_udp_ ~sw ~env ~port ~fn in
  match swo with None -> Switch.run @@ fun sw -> go ~sw | Some sw -> go ~sw

let read_lines_exn ~buf ~on_line =
  while Read.at_end_of_input buf = false do
    on_line (Read.line buf);
    Fiber.yield ()
  done;
  ()

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
