module StrMap = Map.Make (String)

let handle_msg ~dict:(insert, get) ~msg ~addr:_ ~reply =
  let partition_msg (key_rev, value_rev, is_insert_msg) c =
    let open List in
    match (c = '=', is_insert_msg) with
    | true, false -> (key_rev, value_rev, true)
    | false, false -> (c :: key_rev, value_rev, is_insert_msg)
    | _ -> (key_rev, c :: value_rev, is_insert_msg)
  in
  let a, b, is_insert_msg =
    String.to_seq msg |> Seq.fold_left partition_msg ([], [], false)
    |> fun (k, v, i) ->
    List.(rev k |> to_seq |> String.of_seq, rev v |> to_seq |> String.of_seq, i)
  in
  if is_insert_msg then insert a b else reply (a ^ "=" ^ get a)

let listen ~env ~port =
  let version = "caml_unusual_db" in
  let dict = ref StrMap.(empty) in
  let insert key value = dict := StrMap.add key value !dict in
  let get = function
    | "version" -> version
    | k -> ( try StrMap.find k !dict with _ -> "")
  in
  Server.listen_udp ~env ~port ~fn:(handle_msg ~dict:(insert, get))
