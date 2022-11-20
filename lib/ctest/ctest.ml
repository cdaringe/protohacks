let run cases =
  let open Alcotest in
  run "tests"
    (List.mapi
       (fun i (case_name, f) ->
         (Format.sprintf "suite_%i%!" i, [ test_case case_name `Quick f ]))
       cases)
