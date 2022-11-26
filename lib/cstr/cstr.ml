let map_words fn s =
  s |> String.split_on_char ' ' |> List.map fn |> String.concat " "

let to_of_chars f s =
  s |> String.to_seq |> List.of_seq |> f |> List.to_seq |> String.of_seq
