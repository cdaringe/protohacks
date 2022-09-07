(* 0. smoke test *)
(* let _ = Eio_main.run @@ fun env -> Echolib.listen ~env ~port:10000 *)

(* 1. primetime *)
let _ = Eio_main.run @@ fun env -> Primetime.listen ~env ~port:10000
(* let _ = Primetime.test () *)
