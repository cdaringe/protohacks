let rec fold_while f acc = function
  | [] -> acc
  | e :: l -> (
      let acc, cont = f acc e in
      match cont with `Stop -> acc | `Continue -> fold_while f acc l)
