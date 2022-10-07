module StrMap = Map.Make (String)

let handle_msg ~dict:(insert, get) ~msg ~addr:_ ~reply =
  let is_insert_msg = ref false in
  let partition_kv ((key_rev, value_rev) as acc) c =
    let open List in
    match (c = '=', !is_insert_msg) with
    | true, false ->
        is_insert_msg := true;
        acc
    | false, false -> (c :: key_rev, value_rev)
    | _ -> (key_rev, c :: value_rev)
  in
  let a, b =
    String.to_seq msg |> Seq.fold_left partition_kv ([], []) |> fun (kr, vr) ->
    List.(rev kr |> to_seq |> String.of_seq, rev vr |> to_seq |> String.of_seq)
  in
  (* Eio.traceln "%s,%s,%s" (if !is_insert_msg then "insert" else "get") a b; *)
  Eio.traceln "%s,%s" (if !is_insert_msg then "insert" else "get") a;
  match !is_insert_msg with
  | true -> insert a b
  | false ->
      let value = get a in
      (* Eio.traceln "value: %s" value; *)
      reply (a ^ "=" ^ value)

let listen ~env ~port =
  let version = "caml_unusual_db" in
  let dict = ref StrMap.(empty) in
  let insert key value = dict := StrMap.add key value !dict in
  let get = function
    | "version" -> version
    | k -> (
        try StrMap.find k !dict
        with _ ->
          Eio.traceln "%s not found" k;
          "")
  in
  Server.listen_udp ~env ~port ~fn:(handle_msg ~dict:(insert, get))
