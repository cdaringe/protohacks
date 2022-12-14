module type Int = sig
  include Cnum.Num

  val min_int : t
  val max_int : t
end

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a
