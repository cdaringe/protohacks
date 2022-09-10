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

module Res = struct
  let send flow mean =
    let b = Bytes.create 4 in
    Bytes.set_int32_be b 0 (Int64.to_int32 mean);
    Server.send_bytes ~flow b
end

let handle_stream flow _ =
  let module Ledger = Kvtree.T (Int32) in
  let data = ref Ledger.Leaf in
  let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
  try
    while true do
      match Req.of_reader reader with
      | Req.Insert { time; price } -> (
          match !data with
          | Ledger.Leaf -> data := Ledger.init time price
          | Ledger.Node node -> Ledger.insert time price node |> ignore)
      | Req.Query { min; max } -> (
          match !data with
          | Ledger.Leaf -> raise @@ Failure "no data"
          | Ledger.Node _ as t ->
              let module IntMean = Cagg.Mean (Int64) in
              let acc = ref @@ IntMean.init () in
              let on_visit Ledger.{ value; _ } =
                acc := IntMean.acc !acc (Int64.of_int32 value)
              in
              Ledger.(
                match find_node ~min ~max t with
                | Some n -> visit on_visit ~min ~max n
                | None -> ());
              Res.send flow @@ IntMean.agg !acc)
    done
  with End_of_file -> ()

let listen = Server.listen ~fn:handle_stream
