open Eio
open Server

let upstream_ip = ref None

let connect_upstream ~sw ~env ~upstream:(url, port) =
  let net = Eio.Stdenv.net env in
  let ip =
    match !upstream_ip with
    | Some x -> x
    | None -> (
        let ip =
          List.hd @@ Net.getaddrinfo ~service:(Int.to_string port) env#net url
        in
        match ip with
        | `Tcp (ip, _) ->
            upstream_ip := Some ip;
            ip
        | _ -> failwith "unable to get upstream ip")
  in
  Eio.Net.connect ~sw net (`Tcp (ip, port))

module Boguscoin = struct
  let is_alpha_num c =
    let code = Char.code c in
    (code >= 48 && code <= 57) || (code >= 65 && code <= 122)

  let is_coin s =
    let len = String.length s in
    len >= 26 && len <= 35
    && s.[0] = '7'
    && String.to_seq s |> List.of_seq |> List.for_all is_alpha_num
end

module Tony = struct
  let evil = "7YWHMfk9JZe0LM0g1ZauHuiSxhI"

  let swap_tony s =
    let swap_coin s' = if Boguscoin.is_coin s' then evil else s' in
    s |> String.split_on_char ' ' |> List.map swap_coin |> String.concat " "
end

let proxy_lines ~src ~dest _ =
  let on_line line = send_line ~flow:dest (Tony.swap_tony line) in
  try read_lines ~flow:src ~on_line with _ -> ()

let proxy ~up ~down _ =
  Fiber.first (proxy_lines ~src:down ~dest:up) (proxy_lines ~src:up ~dest:down)

let handle_socket ~sw ~env ~upstream down _addr =
  let up = connect_upstream ~sw ~env ~upstream in
  let finally _ =
    let close t _ = try Flow.shutdown t `All with _ -> () in
    Fiber.both (close up) (close down)
  in
  Fun.protect ~finally (proxy ~up ~down)

let listen ~env ~port ~upstream =
  Switch.run (fun sw ->
      Server.listen ~env ~port ~fn:(handle_socket ~sw ~env ~upstream) ())
