let err_hex = [ 0x10; 0x03; 0x62; 0x61; 0x64 ]

let case_string l =
  let b = Bytes.of_seq (List.map Char.chr l |> List.to_seq) in
  String.of_bytes b

let _case_buf l = Eio.Buf_read.of_string (case_string l)

(* let test_lowercase () =
   Alcotest.(check string)
     "same string"
     "SlimyDev161' was not correct (expected '[BrownEdward191] Please pay the \
      ticket price of 15 Boguscoins to one of these addresses: \
      7YWHMfk9JZe0LM0g1ZauHuiSxhI 7YWHMfk9JZe0LM0g1ZauHuiSxhI \
      7YWHMfk9JZe0LM0g1ZauHuiSxhI"
     (t
        "SlimyDev161' was not correct (expected '[BrownEdward191] Please pay \
         the ticket price of 15 Boguscoins to one of these addresses: \
         7YWHMfk9JZe0LM0g1ZauHuiSxhI 7FdD7oOGgQGd2MppdZxSjyOANI8SNH7jrp \
         7YWHMfk9JZe0LM0g1ZauHuiSxhI") *)

let _client_msg =
  let pp_string ppf x = Fmt.pf ppf "%S" (Speed.Msg.show_client_msg x) in
  Alcotest.testable pp_string ( = )

let _server_msg =
  let pp_string ppf x = Fmt.pf ppf "%S" (Speed.Msg.show_server_msg x) in
  Alcotest.testable pp_string ( = )

let _buff_string =
  let pp_string ppf x = Fmt.pf ppf "%S" (Eio.Buf_write.serialize_to_string x) in
  Alcotest.testable pp_string ( = )

let case_parse_server_err_msg () =
  let expected = case_string err_hex in
  let b = Eio.Buf_write.create 1000 in
  Speed.Msg.(Server.prepare_msg b (Error { msg = "bad" }));
  Alcotest.(check string)
    "server msg"
    (Eio.Buf_write.serialize_to_string b)
    expected

let () =
  let open Alcotest in
  run "speed"
    [ ("error msg", [ test_case "end" `Quick case_parse_server_err_msg ]) ]
