let () = Printexc.record_backtrace true

(* 0. smoke test *)
(* let _ = Eio_main.run @@ fun env -> Echolib.listen ~env ~port:10000 *)

(* 1. primetime *)
(* let _primetime _ = Eio_main.run @@ fun env -> Primetime.listen ~env ~port:10000 *)

(* 2. means to an end *)
(* let _means _ = Eio_main.run @@ fun env -> Means.listen ~env ~port:10000 *)

(* 3. budget chat *)
(* let budget _ = Eio_main.run @@ fun env -> Budgetchat.listen ~env ~port:9999 *)

(* 3. budget chat *)
let main _ = Eio_main.run @@ fun env -> Unusual_db.listen ~env ~port:8888 ()

(* main *)
let _ = main ()
