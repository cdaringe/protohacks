module type Num = sig
  type t

  val add : t -> t -> t
  val div : t -> t -> t
  val one : t
  val zero : t
end
