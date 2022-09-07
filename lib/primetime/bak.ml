(* let big_int_str =
     "434853896852639606218636981160816279029395220139740510962619477"

   let big_float = Float.of_string big_int_str
   let big_json_float = `Float big_float

   let test _ =
     print_endline
     @@ Printf.sprintf "is integer ?: %b" (Float.is_integer big_float);
     match big_json_float |> Num.of_yojson with
     | Ok t -> print_endline @@ Num.pp t
     | Error e -> print_endline e *)
