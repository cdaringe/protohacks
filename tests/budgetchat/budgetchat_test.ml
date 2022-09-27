open Eio

let run f = Eio_main.run (fun env -> Switch.run (fun sw -> f ~env ~sw))

let main ~env ~sw =
  let pool, enqueue, _swo = Budgetchat.create_msg_queue_env sw in
  let _net = Eio_mock.Net.make "mocknet" in
  let in_fiber socket =
    Fiber.fork ~sw (fun _ -> Budgetchat.handle_socket ~enqueue ~pool socket ())
  in
  let chat_user_socket_1 = Eio_mock.Flow.make "alice" in
  let chat_user_socket_2 = Eio_mock.Flow.make "bob" in
  let p1, resolver1 = Promise.create () in
  Eio_mock.Flow.on_read chat_user_socket_1
    [
      `Yield_then (`Return "alice\n");
      `Yield_then (`Return "alice_outbound_1\n");
      `Yield_then (`Return "alice_outbound_2\n");
      `Run (fun () -> Promise.await p1 |> fun _ -> "alice_outbound_3");
      `Yield_then (`Raise End_of_file);
    ];
  Eio_mock.Flow.on_read chat_user_socket_2
    [
      `Yield_then (`Return "bob\n");
      `Yield_then (`Return "bob_outbound_1\n");
      `Run
        (fun _ ->
          Promise.resolve resolver1 () |> fun _ ->
          let clock = Eio.Stdenv.clock env in
          Eio.Time.sleep clock 1.0;
          "bob_outbound_2");
      `Yield_then (`Return "bob_outbound_3\n");
      `Yield_then (`Raise End_of_file);
    ];
  in_fiber chat_user_socket_1;
  in_fiber chat_user_socket_2;
  ()

let _ = run main

(*
Fiber.fork ~sw (fun _ ->
      Fiber.yield ();
      Fiber.yield ();
      Fiber.yield ();
      Fiber.yield ();
      let q = Queue.create () in
      let msgs = [ "test_user\n"; "guten\n"; "tag\n"; "amigos!\n" ] in
      msgs |> List.to_seq |> Queue.add_seq q;
      let rec process_cmd _ =
        match Queue.take_opt q with
        | Some cmd ->
            Eio.Flow.copy_string cmd flow;
            Fiber.yield ();
            process_cmd ()
        | None -> Eio.Flow.close flow
      in
      process_cmd ());
*)
