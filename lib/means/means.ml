module Docket (Num : Cint.Int) = struct
  type t

  type 'v tree = Leaf | Node of 'v node

  and 'v node = {
    key : Num.t;
    value : 'v;
    left : 'v tree ref;
    right : 'v tree ref;
  }

  let init key value = Node { key; value; left = ref Leaf; right = ref Leaf }

  let rec insert k v n =
    let child = if k < n.key then n.left else n.right in
    match !child with Leaf -> child := init k v | Node n' -> insert k v n'

  let find ?(min = Num.min_int) ?(max = Num.max_int) n =
    let in_range n = n.key >= min && n.key <= max in
    let rec find = function
      | Leaf -> None
      | Node n ->
          if in_range n then Some n
          else
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
end

module Req = struct
  type t =
    | Query of { min : int32; max : int32 }
    | Insert of { time : int32; price : int32 }

  let bytes_to_int cl =
    cl |> List.map Char.chr |> List.to_seq |> Bytes.of_seq |> fun b ->
    Bytes.get_int32_be b 0

  let rec parse_int_bytes l b =
    if List.length l < 4 then
      let code = Char.code @@ Eio.Buf_read.any_char b in
      parse_int_bytes (code :: l) b
    else l |> List.rev |> bytes_to_int

  let of_reader b =
    let c = Eio.Buf_read.any_char b in
    let a = parse_int_bytes [] b in
    let b = parse_int_bytes [] b in
    match c with
    | 'Q' -> Query { min = a; max = b }
    | 'I' -> Insert { time = a; price = b }
    | c -> raise @@ Failure (Printf.sprintf "'%c' invalid msg type" c)
end

let handle_stream flow _ =
  let module Docket32 = Docket (Int32) in
  let data = ref Docket32.Leaf in
  let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
  try
    while true do
      match Req.of_reader reader with
      | Req.Insert { time; price } -> (
          match !data with
          | Docket32.Leaf -> data := Docket32.init time price
          | Docket32.Node node -> Docket32.insert time price node |> ignore)
      | Req.Query { min; max } -> (
          match !data with
          | Docket32.Leaf -> raise (Failure "no data")
          | Docket32.Node _ as t ->
              let module IntMean = Cagg.Mean (Int64) in
              let acc = ref @@ IntMean.init () in
              let on_visit Docket32.{ value; _ } =
                acc := IntMean.acc !acc (Int64.of_int32 value)
              in
              Docket32.(
                match find ~min ~max t with
                | Some n -> visit_in_range on_visit min max n
                | None -> ());
              let mean = IntMean.agg !acc in
              let b = Bytes.create 4 in
              Bytes.set_int32_be b 0 (Int64.to_int32 mean);
              Server.send_bytes ~flow b;
              ())
    done
  with End_of_file -> ()

let listen = Server.listen ~fn:handle_stream
