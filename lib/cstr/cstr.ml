let map_words fn s =
  s |> String.split_on_char ' ' |> List.map fn |> String.concat " "
