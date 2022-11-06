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

let sort_by_f f l =
  let compare_fn a b = compare (f a) (f b) in
  List.sort compare_fn l

let sort_by_fst l = sort_by_f fst l
let sort_by_snd l = sort_by_f snd l
