(* open Eio *)
open Lrcp

let test_exn expected fn =
  try
    fn () |> ignore;
    Alcotest.(check string) "" "expected exn, got none" ""
  with e ->
    let msg = Printexc.to_string e in
    print_endline msg;
    Alcotest.(check string) "" expected msg

let serde_cases_inputs =
  [ "/data/123/0/hello/"; "/connect/123/"; "/ack/123/145/"; "/close/123/" ]

let serde_case input _ =
  Alcotest.(check string) "" (In.parse input |> Out.of_t) input

let serde_cases =
  List.map
    (fun input ->
      let case_name = "ser[de] case: " ^ input in
      (case_name, serde_case input))
    serde_cases_inputs

let escape_case_foobar = ({ii|foo\/bar\/baz|ii}, {ii|foo/bar/baz|ii})
let escape_case_doubletrouble = ({ii|foo\\bar\\baz|ii}, {ii|foo\bar\baz|ii})
let msg_bad_data_illegal_slashes = {ii|/data/1/2/illegal//data/|ii}

let test_bad_data_slashes _ =
  test_exn "Failure(\"expected escape for '/' char\")" (fun _ ->
      Lrcp.In.parse msg_bad_data_illegal_slashes)

let escape_test (escaped, unescaped) () =
  Alcotest.(check string)
    (Fmt.str "unescaping... %s -> %s" escaped unescaped)
    (unescape escaped) unescaped;
  Alcotest.(check string) "escaping..." (escape unescaped) escaped

let () =
  Ctest.run
    (List.flatten
       [
         serde_cases;
         [
           ("foobar", escape_test escape_case_foobar);
           ("doubletrouble", escape_test escape_case_doubletrouble);
           ("illegal", test_bad_data_slashes);
         ];
       ])
