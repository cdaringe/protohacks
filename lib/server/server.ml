open Eio
open Eio.Net

let listen ~env ~port ~fn =
  Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Ipaddr.V4.any, port) in
  let socket = listen ~sw ~backlog:1000 net addr in
  let count = ref 0 in
  let on_error e =
    traceln "error handling connection: %a\n\n%s" Fmt.exn e
      (Printexc.to_string e)
  in
  traceln "server listening on %i" port;
  let on_fork a b =
    incr count;
    let id = !count in
    print_endline @@ Printf.sprintf "[socket %i]: start" id;
    fn a b;
    print_endline @@ Printf.sprintf "[socket %i]: end" id
  in
  while true do
    accept_fork socket ~sw ~on_error on_fork
  done

let read_lines ~flow ~on_line =
  let open Eio in
  let buf = Buf_read.of_flow flow ~initial_size:100 ~max_size:1_000_000 in
  try
    while Buf_read.eof_seen buf = false do
      on_line (Buf_read.line buf)
    done;
    traceln "lines all read"
  with End_of_file -> ()

module Write = Eio.Buf_write

let send_ndjson ~tojson ~flow t =
  let ndjson = tojson t ^ "\n" in
  let write_flush t =
    Write.flush t;
    Write.string t ndjson;
    Write.flush t
  in
  Write.with_flow flow write_flush
