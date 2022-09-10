let () = Printexc.record_backtrace true

(* 0. smoke test *)
(* let _ = Eio_main.run @@ fun env -> Echolib.listen ~env ~port:10000 *)

(* 1. primetime *)
(* let _ = Eio_main.run @@ fun env -> Primetime.listen ~env ~port:10000 *)

(* 2. means to an end *)
let _ = Eio_main.run @@ fun env -> Means.listen ~env ~port:10000
