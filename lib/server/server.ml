open Eio
open Eio.Net
module Read = Eio.Buf_read
module Write = Eio.Buf_write

let listen ~env ~port ~fn =
  Switch.run @@ fun sw ->
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

let read_lines_exn ~buf ~on_line =
  while Read.eof_seen buf = false do
    on_line (Read.line buf)
  done;
  traceln "lines all read"

let read_bytes ~flow num_bytes =
  let r = Read.of_flow flow ~initial_size:num_bytes ~max_size:num_bytes in
  r

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

let send_ndjson ~tojson ~flow t =
  let ndjson = tojson t ^ "\n" in
  let write_flush t =
    (* probably a frivolous flush, but protohackers seems to be less
       finicky with it? could be blind luck. *)
    Write.flush t;
    Write.string t ndjson;
    Write.flush t
  in
  Write.with_flow flow write_flush
