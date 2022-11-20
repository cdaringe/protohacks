open Eio

(*
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

*)

module S = Lrcp_session.SessionManager

(* let server_addr = `Udp (Eio.Net.Ipaddr.V4.any, 10000) *)
let client_addr = `Udp (Eio.Net.Ipaddr.V4.any, 20000)

let session_test_x actions =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let cli_msgs, srv_msgs = actions in
  (* let rem_srv = ref srv_msgs in *)
  let rem_srv = ref srv_msgs in
  let clock = Eio.Stdenv.clock env in
  (* let sleep f = Eio.Time.sleep clock f in *)
  let on_event = function S.Open -> () | S.Data _ -> () | S.Close -> () in
  let sessionm = S.init ~on_event ~sw ~clock in
  let handler = S.handle_msg sessionm in
  let reply m =
    (* traceln "%s" _m  *)
    let tl = List.tl !rem_srv in
    let hd = List.hd !rem_srv in
    if String.equal hd m then rem_srv := tl
    else failwith (Fmt.str "msg mismatch: (got %s) / (wanted %s)" m hd)
  in
  let rec process f l =
    match l with
    | [] -> ()
    | x :: xs ->
        f x;
        process f xs
  in
  let f msg = handler ~msg ~addr:client_addr ~reply in
  process f cli_msgs;
  ()

(*
let server_addr = `Udp (Eio.Net.Ipaddr.V4.any, 10000)
let client_addr = `Udp (Eio.Net.Ipaddr.V4.any, 20000)

let app_sw ?(port = 10000) ~env is_alive =
  Fiber.first
    (fun _ -> Lrcp_app.listen ~env ~port ())
    (fun _ ->
      let clock = Eio.Stdenv.clock env in
      while !is_alive do
        Eio.Time.sleep clock 0.1;
        Fiber.yield ()
      done)

let sender env msg =
   Eio.Switch.run @@ fun sw ->
   let socket = Eio.Net.datagram_socket ~sw (Eio.Stdenv.net env) client_addr in
   let buf = Cstruct.of_string msg in
   Eio.Net.send socket server_addr buf;
   traceln "Sent %d bytes (%s)" (Cstruct.length buf) msg;
   Eio.Net.close socket *)

(* let app_test _ = Eio_main.run @@ fun env -> (
     let is_alive = ref true in
     Fiber.both
       (fun _ -> app_sw ~env is_alive)
       (fun _ ->
         )
   ) *)

let partition l =
  List.fold_right
    (fun (dir, m) (ts, tc) ->
      match dir with `ToServer -> (m :: ts, tc) | `ToClient -> (ts, m :: tc))
    l ([], [])

let actions =
  [
    (`ToServer, "/connect/12345/");
    (`ToClient, "/ack/12345/0/");
    (`ToServer, "/data/12345/0/Hello, world!/");
    (`ToClient, "/ack/12345/13/");
    (`ToServer, "/close/12345/");
    (`ToClient, "/close/12345/");
  ]
  |> partition

let () = session_test_x actions
