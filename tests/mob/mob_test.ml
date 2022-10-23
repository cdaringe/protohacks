let t = Mob.Tony.swap_tony

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

let test_lowercase () =
  Alcotest.(check string)
    "same string" "Send refunds to 7YWHMfk9JZe0LM0g1ZauHuiSxhI please"
    (t "Send refunds to 7xnMb4WGfLY2seX5eEiTQZSqjwJGazcFxDU please")

let () =
  let open Alcotest in
  run "mob" [ ("tony", [ test_case "end" `Quick test_lowercase ]) ]
