(* open Eio

   let cli ~stdin ~stdout =
     let buf = Eio.Buf_read.of_flow stdin ~initial_size:100 ~max_size:1_000_000 in
     while true do
       let line = Eio.Buf_read.line buf in
       traceln "> %s" line;
       match line with
       | "h" | "help" -> Eio.Flow.copy_string "It's just an example\n" stdout
       | x -> Eio.Flow.copy_string (Fmt.str "Unknown command %S\n" x) stdout
     done

   let _ = Eio_main.run @@ fun env ->
    cli ~stdin:(Stdenv.stdin env) ~stdout:(Stdenv.stdout env)
*)
let _ = Eio_main.run @@ fun env -> Echolib.listen ~env
