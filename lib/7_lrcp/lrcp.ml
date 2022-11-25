type string_list = string list [@@deriving show]
type data_tuple = int * int * string

type t =
  | Connect of int
  | Close of int
  | Ack of (int * int)
  | Data of data_tuple

let unslash s =
  let chars = String.to_seq s |> List.of_seq in
  (* valid: \/ *)
  (* valid: \\ *)
  let rec aux ~is_escaping l =
    match l with
    | [] -> []
    | c :: xs -> (
        match c with
        | '/' ->
            if is_escaping then c :: aux xs ~is_escaping:false
            else failwith "expected escape for '/' char"
        | '\\' ->
            if is_escaping then c :: aux xs ~is_escaping:false
            else aux xs ~is_escaping:true
        | x ->
            if is_escaping then failwith "invalid escape for char"
            else x :: aux ~is_escaping:false xs)
  in
  aux ~is_escaping:(String.starts_with ~prefix:"\\" s) chars
  |> List.to_seq |> String.of_seq

let unescape_fslash = CCString.replace ~sub:"\\/" ~by:"/"
let unescape_bslash = CCString.replace ~sub:"\\\\" ~by:"\\"
let unescape s = unslash s
(* s |> unescape_bslash |> unescape_fslash *)

let escape_fslash = CCString.replace ~sub:{x|/|x} ~by:{x|\/|x}
let escape_bslash = CCString.replace ~sub:{x|\|x} ~by:{x|\\|x}
let escape s = s |> escape_bslash |> escape_fslash

module In = struct
  exception Parse_msg of string

  let is_empty_str = String.equal ""
  let is_non_empty_str s = is_empty_str s |> not

  let int_gt_0 x =
    let i = int_of_string x in
    if i > 0 then i else failwith "invalid gt_0 int"

  let safe_int_of_string x =
    let i = int_of_string x in
    if i > 0 && i < 2147483648 then i else failwith "invalid <2.4b int"

  let parse str =
    let is_valid_begend s =
      String.starts_with ~prefix:"/" s && String.ends_with ~suffix:"/" s
    in
    if is_valid_begend str then
      let parts =
        String.split_on_char '/' str |> List.filter is_non_empty_str
      in
      match parts with
      | [ "connect"; sess ] -> Connect (int_gt_0 sess)
      | "data" :: _ ->
          Scanf.sscanf str "/data/%u/%u/" (fun id pos ->
              let payload =
                CCString.replace ~which:`Left
                  ~sub:(Fmt.str "/data/%i/%i/" id pos)
                  ~by:"" str
              in
              match CCString.chop_suffix ~suf:"/" payload with
              | None -> failwith "bad data datagram"
              | Some data ->
                  (* Eio.traceln "scanf:%s%!" data; *)
                  Data (id, pos, data |> unescape))
      | [ "ack"; sess; len ] -> Ack (int_gt_0 sess, safe_int_of_string len)
      | [ "close"; sess ] -> Close (int_gt_0 sess)
      | _ -> raise (Parse_msg "invalid datagram: keyword not detected")
    else raise (Parse_msg "invalid datagram: non matching keyword")
end

module Out = struct
  let of_t t =
    let parts =
      match t with
      | Connect s -> [ "connect"; string_of_int s ]
      | Close s -> [ "close"; string_of_int s ]
      | Ack (s, l) -> [ "ack"; string_of_int s; string_of_int l ]
      | Data (s, p, d) ->
          [ "data"; string_of_int s; string_of_int p; d |> escape ]
    in
    "/" ^ String.concat "/" parts ^ "/"
end
