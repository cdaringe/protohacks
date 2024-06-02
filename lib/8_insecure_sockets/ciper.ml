module CipherByte = struct
  let reverse_bits b =
    let rec aux b acc i =
      if i >= 0 then
        let bit = (b lsr i) land 1 in
        let acc = (acc lsl 1) lor bit in
        aux b acc (i - 1)
      else
        acc
    in
    aux b 0 7

end
(*
module Cipher = struct
  type t = list

end *)

end
