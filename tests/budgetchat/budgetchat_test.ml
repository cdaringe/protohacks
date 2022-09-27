open Eio

let run f = Eio_main.run (fun env -> Switch.run (fun sw -> f ~env ~sw))

let main ~env ~sw =
  let pool, enqueue = Budgetchat.create_msg_queue_env () in
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
          Eio.Time.sleep clock 0.1;
          "bob_outbound_2");
      `Yield_then (`Return "bob_outbound_3\n");
      `Yield_then (`Raise End_of_file);
    ];
  in_fiber chat_user_socket_1;
  in_fiber chat_user_socket_2;
  ()

let _ = run main
