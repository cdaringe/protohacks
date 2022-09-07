module Uint = Stdint.Uint128

module Num = struct
  type t = NInt of Uint.t | NFlt of float

  let u = Uint.of_int
  let u_of_string = Uint.of_string
  let zero = u 0
  let one = u 1
  let two = u 2
  let three = u 3
  let five = u 5
  let six = u 6

  let pp t =
    match t with NInt x -> Uint.to_string x | NFlt x -> Float.to_string x

  let max_uint_flt = Uint.(to_float max_int)
  let min_uint_flt = Uint.(to_float min_int)

  let is_json_big_int f =
    not (f < min_uint_flt || f > max_uint_flt || not (Float.is_integer f))

  let to_yojson = function
    | NInt i -> `Float (Uint.to_float i)
    | NFlt f -> `Float f

  let of_yojson = function
    | `Int i -> Ok (NFlt (Float.of_int i)) (* Ok (NInt (Uint.of_int i)) *)
    | `Float f ->
        Ok (if is_json_big_int f then NInt (Uint.of_float f) else NFlt f)
    | json -> Error (Yojson.Safe.to_string json)

  let check_prime (t : t) =
    match t with
    | NFlt x -> (
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
                if rem x !i = 0. || rem x (add !i 2.) = 0. then
                  known_unprime := true
                else i := add !i 6.
              done;
              if !known_unprime then false else true)
    | NInt x -> (
        let open Uint in
        match x with
        | x when x <= one -> false
        | x when x <= three -> true
        | x ->
            if rem x two = zero || rem x three = zero then false
            else
              let i = ref five in
              let known_unprime = ref false in
              while !known_unprime = false && mul !i !i <= x do
                if rem x !i = zero || rem x (add !i two) = zero then
                  known_unprime := true
                else i := add !i six
              done;
              if !known_unprime then false else true)
end
