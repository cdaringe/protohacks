let () = Printexc.record_backtrace true

(* 0. smoke test *)
(* let _ = Eio_main.run @@ fun env -> Echolib.listen ~env ~port:10000 *)

(* 1. primetime *)
(* let _primetime _ = Eio_main.run @@ fun env -> Primetime.listen ~env ~port:10000 *)

(* 2. means to an end *)
(* let _means _ = Eio_main.run @@ fun env -> Means.listen ~env ~port:10000 *)

(* 3. budget chat *)
(* let budget _ = Eio_main.run @@ fun env -> Budgetchat.listen ~env ~port:9999 *)

(* 4. unusual db *)
(* let main _ = Eio_main.run @@ fun env -> Unusual_db.listen ~env ~port:8888 () *)

(* 5. mob *)
let main _ =
  Eio_main.run @@ fun env ->
  Mob.listen ~env ~port:9999 ~upstream:("chat.protohackers.com", 16963)

(* main *)
let _ = main ()
