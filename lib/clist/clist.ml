let rec fold_while f acc = function
  | [] -> acc
  | e :: l -> (
      let acc, cont = f acc e in
      match cont with `Stop -> acc | `Continue -> fold_while f acc l)

let drop i l =
  let rec inner rem l' =
    if rem = 0 then l'
    else match l' with _ :: xs -> inner (rem - 1) xs | x -> x
  in
  inner i l
