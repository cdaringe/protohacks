let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alphanum c = is_alpha c || is_digit c
let is_alphanum_str s = s |> String.to_seq |> Seq.for_all is_alphanum
