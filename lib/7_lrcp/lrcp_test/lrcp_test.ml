open Lrcp

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

let () = Ctest.run serde_cases
