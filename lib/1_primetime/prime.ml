let check_prime x =
  let open Float in
  match x with
  | x when not @@ Float.is_integer x -> false
  | x when x <= 1. -> false
  | x when x <= 3. -> true
  | x ->
      if rem x 2. = 0. || rem x 3. = 0. then false
      else
        let i = ref 5. in
        let known_unprime = ref false in
        while !known_unprime = false && mul !i !i <= x do
          if rem x !i = 0. || rem x (add !i 2.) = 0. then known_unprime := true
          else i := add !i 6.
        done;
        if !known_unprime then false else true
