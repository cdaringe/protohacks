let singleton ~conflict_msg ~set rref =
  match !rref with
  | None -> ( match set () with None -> () | Some x -> rref := Some x)
  | Some _ -> failwith conflict_msg
