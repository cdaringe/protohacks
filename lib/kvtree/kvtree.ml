module T (Num : Cint.Int) = struct
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

  let find_node ?(min = Num.min_int) ?(max = Num.max_int) n =
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

  let rec visit (on_visit : 'a node -> unit) ?(min = Num.min_int)
      ?(max = Num.max_int) n =
    let { key; left; right; _ } = n in
    let is_out_of_range = key < min || key > max in
    if is_out_of_range then () else on_visit n;
    let visit' = function Node x -> visit on_visit ~min ~max x | _ -> () in
    visit' !left;
    visit' !right
end
