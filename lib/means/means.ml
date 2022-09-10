module Errors = struct
  type t = InvalidPayload of string
end

module Docket = struct
  open Int32

  type t

  let rec insert key value = function
    | [] -> [ (key, value) ]
    | (k, v) :: xs as l ->
        let open List in
        if key < k then (key, value) :: l else (k, v) :: insert key value xs

  let in_range min max =
    let fn acc (k, v) =
      if k < min then (acc, `Continue)
      else if k <= max then ((k, v) :: acc, `Continue)
      else (acc, `Stop)
    in
    Clist.fold_while fn []

  let sum = List.(fold_left add zero)

  let mean t =
    let open List in
    let len = length t in
    if len = 0 then Int32.zero else div (sum t) (of_int @@ len)

  let rec values = function [] -> [] | x :: xs -> snd x :: values xs
end

module Req = struct
  type t =
    | Query of { min : int32; max : int32 }
    | Insert of { time : int32; price : int32 }
  [@@deriving show, yojson { strict = false }]

  let to_int s = s |> String.to_bytes |> fun b -> Bytes.get_int32_be b 4

  let bytes_to_int cl =
    cl |> List.map Char.chr |> List.to_seq |> Bytes.of_seq |> fun b ->
    (* Eio.traceln "bytes_to_string: %s" (Bytes.to_string b); *)
    Bytes.get_int32_be b 0

  type int_list = int list [@@deriving show]

  let rec parse_int_bytes l b =
    if List.length l < 4 then
      let c = Eio.Buf_read.any_char b in
      let code = Char.code c in
      (* Eio.traceln "char: %c, code: %i" c code; *)
      parse_int_bytes (code :: l) b
    else l |> List.rev

  let q_of_reader b =
    let min = parse_int_bytes [] b and max = parse_int_bytes [] b in
    Query { min = min |> bytes_to_int; max = max |> bytes_to_int }

  let i_of_reader b =
    let time = parse_int_bytes [] b in
    let price = parse_int_bytes [] b in
    (* Eio.traceln "%s, %s" (show_int_list time) (show_int_list price); *)
    Insert { time = bytes_to_int time; price = bytes_to_int price }

  let of_reader buf_r =
    (* let trace = Eio.traceln in *)
    let mode = Eio.Buf_read.any_char buf_r in
    (* trace "%c: query type" mode; *)
    let tap_show r =
      (* trace "%s" (show r); *)
      r
    in
    match mode with
    | 'Q' -> q_of_reader buf_r |> tap_show |> Result.ok
    | 'I' -> i_of_reader buf_r |> tap_show |> Result.ok
    | c ->
        Error (Errors.InvalidPayload (Printf.sprintf "'%c' invalid msg type" c))
end

module Res = struct
  type t = { req_method : string; [@key "method"] prime : bool }
  [@@deriving show, yojson { strict = false }]
end

let count = ref 0

let handle_stream flow _ =
  let data = ref [] in
  let reader = Eio.Buf_read.of_flow flow ~max_size:9 in
  try
    while true do
      count := !count + 1;
      let _ =
        if !count mod 1000 = 0 then Eio.traceln "count: %i" !count else ()
      in
      match Req.of_reader reader with
      | Ok (Req.Insert { time; price }) ->
          data := Docket.insert time price !data
      | Ok (Req.Query { min; max }) ->
          Docket.(in_range min max !data |> values |> mean) |> fun mean ->
          let b = Bytes.create 4 in
          Bytes.set_int32_be b 0 mean;
          Server.send_bytes ~flow b;
          ()
      | Error (Errors.InvalidPayload msg) -> Eio.traceln "Parse failed: %s" msg
    done
  with End_of_file -> ()

let listen = Server.listen ~fn:handle_stream
