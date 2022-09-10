module Errors = struct
  type t = InvalidPayload of string
end

module Docket = struct
  open Int32

  type 'v tree = Leaf | Node of 'v node [@@deriving show]

  and 'v node = {
    key : int32;
    value : 'v;
    left : 'v tree ref;
    right : 'v tree ref;
  }
  [@@deriving show]

  type t

  let init key value = Node { key; value; left = ref Leaf; right = ref Leaf }

  let rec insert k v n =
    let child = if k < n.key then n.left else n.right in
    match !child with Leaf -> child := init k v | Node n' -> insert k v n'

  let find_root_in_range min max n =
    let in_range n = n.key >= min && n.key <= max in
    let rec find n =
      match n with
      | Leaf -> None
      | Node n ->
          if in_range n then Some n
          else
            (* let _ = Eio.traceln "-> %i" (Int32.to_int n.key) in *)
            let child = if n.key < min then n.right else n.left in
            find !child
    in
    find n

  let rec visit_in_range (on_visit : 'a node -> unit) min max n =
    let { key; left; right; _ } = n in
    let is_out_of_range = key < min || key > max in
    if is_out_of_range then () else on_visit n;
    (match !left with Node n' -> visit_in_range on_visit min max n' | _ -> ());
    (match !right with
    | Node n' -> visit_in_range on_visit min max n'
    | _ -> ());
    ()

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
    Bytes.get_int32_be b 0

  type int_list = int list [@@deriving show]

  let rec parse_int_bytes l b =
    if List.length l < 4 then
      let c = Eio.Buf_read.any_char b in
      let code = Char.code c in
      parse_int_bytes (code :: l) b
    else l |> List.rev

  let q_of_reader b =
    let min = parse_int_bytes [] b and max = parse_int_bytes [] b in
    Query { min = min |> bytes_to_int; max = max |> bytes_to_int }

  let i_of_reader b =
    let time = parse_int_bytes [] b in
    let price = parse_int_bytes [] b in
    Insert { time = bytes_to_int time; price = bytes_to_int price }

  let of_reader buf_r =
    let mode = Eio.Buf_read.any_char buf_r in
    match mode with
    | 'Q' -> q_of_reader buf_r |> Result.ok
    | 'I' -> i_of_reader buf_r |> Result.ok
    | c ->
        Error (Errors.InvalidPayload (Printf.sprintf "'%c' invalid msg type" c))
end

module Res = struct
  type t = { req_method : string; [@key "method"] prime : bool }
  [@@deriving show, yojson { strict = false }]
end

let handle_stream flow _ =
  let sf s = Printf.sprintf s in
  let log s = Eio.traceln "%s" s in
  let data = ref Docket.Leaf in
  let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
  try
    while true do
      match Req.of_reader reader with
      | Ok (Req.Insert { time; price }) -> (
          match !data with
          | Docket.Leaf -> data := Docket.init time price
          | Docket.Node node -> Docket.insert time price node |> ignore)
      | Ok (Req.Query { min; max }) -> (
          match !data with
          | Docket.Leaf -> raise (Failure "no data")
          | Docket.Node _ as t ->
              let total = ref Int64.zero in
              let count = ref 0 in
              let on_visit Docket.{ value; _ } =
                (total := Int64.(add !total (of_int32 value)));
                incr count;
                ()
              in
              Docket.(
                find_root_in_range min max t |> function
                | Some n -> visit_in_range on_visit min max n
                | None -> ());
              let mean =
                let open Int64 in
                if !count = 0 then zero else div !total (of_int !count)
              in
              let b = Bytes.create 4 in
              log
              @@ sf "min,max,mean,count\n%i,%i,%s,%i" (Int32.to_int min)
                   (Int32.to_int max) (Int64.to_string mean) !count;
              Bytes.set_int32_be b 0 (Int64.to_int32 mean);
              Server.send_bytes ~flow b;
              ())
      | Error (Errors.InvalidPayload msg) -> Eio.traceln "Parse failed: %s" msg
    done
  with End_of_file -> ()

let listen = Server.listen ~fn:handle_stream
