open Eio
open Server

module Boguscoin = struct
  let is_coin s =
    let len = String.length s in
    len >= 26 && len <= 35 && s.[0] = '7' && Cchar.is_alphanum_str s

  let evil = "7YWHMfk9JZe0LM0g1ZauHuiSxhI"
  let swap_coin s = if is_coin s then evil else s
  let swap = Cstr.map_words swap_coin
end

let upstream_ip = ref None

let connect_upstream ~sw ~env ~upstream:(url, port) =
  let net = Eio.Stdenv.net env in
  let get_upstream_ip _ = Cnet.get_ip ~env ~service:(Int.to_string port) url in
  let ip, _ = Copt.get_def get_upstream_ip !upstream_ip in
  Eio.Net.connect ~sw net (`Tcp (ip, port))

let proxy_lines src dest _ =
  let on_line line = send_line ~flow:dest (Boguscoin.swap line) in
  try read_lines ~flow:src ~on_line with _ -> ()

let handle_socket ~sw ~env ~upstream down _addr =
  let up = connect_upstream ~sw ~env ~upstream in
  let finally _ =
    let close t _ = try Flow.shutdown t `All with _ -> () in
    Fiber.both (close up) (close down)
  in
  let proxy _ = Fiber.first (proxy_lines down up) (proxy_lines up down) in
  Fun.protect ~finally proxy

let listen ~env ~port ~upstream =
  Switch.run (fun sw ->
      Server.listen ~env ~port ~fn:(handle_socket ~sw ~env ~upstream) ())
