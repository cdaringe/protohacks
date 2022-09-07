module Req = struct
  type t = { number : float; req_method : string [@key "method"] }
  [@@deriving yojson { strict = false }]

  let of_unverified t =
    if t.req_method = "isPrime" then Ok t else Error "bad method"

  let parse t =
    t |> Yojson.Safe.from_string |> of_yojson |> Cresult.map_ok of_unverified
end

module Res = struct
  type t = { req_method : string; [@key "method"] prime : bool }
  [@@deriving show, yojson { strict = false }]

  let to_json t = t |> to_yojson |> Yojson.Safe.to_string
  let of_prime prime = { req_method = "isPrime"; prime }
  let malformed msg = { req_method = msg; prime = false }
end

let handle_stream flow _ =
  let send = Server.send_ndjson ~tojson:Res.to_json ~flow in
  let on_line line =
    Req.parse line |> function
    | Ok req -> Prime.check_prime req.number |> Res.of_prime |> send
    | Error e ->
        let msg = Printf.sprintf "failed parse: %s%!" line in
        send @@ Res.malformed (Printf.sprintf "%s" e);
        raise @@ Failure msg
  in
  Server.read_lines ~flow ~on_line

let listen = Server.listen ~fn:handle_stream
