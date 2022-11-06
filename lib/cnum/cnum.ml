module type Num = sig
  type t

  val add : t -> t -> t
  val div : t -> t -> t
  val one : t
  val zero : t
end

let abs_i value = if value < 0 then -value else value
let abs_f value = if value < 0. then -1. *. value else value
