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
  let rem_srv = ref srv_msgs in
  let clock = Eio.Stdenv.clock env in
  let handler = Lrcp_app.create_lrcp_handler ~clock ~sw in
  let reply m =
    let tl = List.tl !rem_srv in
    let hd = List.hd !rem_srv in
    if String.equal hd m then rem_srv := tl
    else failwith (Fmt.str "msg mismatch: (got %s) / (wanted %s)" m hd)
  in
  let rec process_client_msg f l =
    match l with
    | [] -> ()
    | x :: xs ->
        (* traceln "[test] processing client msg"; *)
        f x;
        Eio.Time.sleep clock 0.05;
        process_client_msg f xs
  in
  let f msg = handler ~msg ~addr:client_addr ~reply in
  process_client_msg f cli_msgs;
  ()

let partition_by_dest l =
  List.fold_right
    (fun (dir, m) (ts, tc) ->
      match dir with `ToServer -> (m :: ts, tc) | `ToClient -> (ts, m :: tc))
    l ([], [])

let _basic_actions =
  [
    (`ToServer, "/connect/12345/");
    (`ToClient, "/ack/12345/0/");
    (`ToServer, "/data/12345/0/Hello, world!/");
    (`ToClient, "/ack/12345/13/");
    (`ToServer, "/close/12345/");
    (`ToClient, "/close/12345/");
  ]
  |> partition_by_dest

(* let () = session_test_x basic_actions *)

let t2 _ =
  session_test_x
    ([
       (`ToServer, "/connect/12345/");
       (`ToClient, "/ack/12345/0/");
       (`ToServer, "/data/12345/0/hello\n/");
       (`ToClient, "/ack/12345/6/");
       (`ToClient, "/data/12345/0/olleh\n/");
       (`ToServer, "/ack/12345/6/");
       (`ToServer, "/data/12345/6/Hello, world!\n/");
       (`ToClient, "/ack/12345/20/");
       (`ToClient, "/data/12345/6/!dlrow ,olleH\n/");
       (`ToServer, "/ack/12345/20/");
       (`ToServer, "/close/12345/");
       (`ToClient, "/close/12345/");
     ]
    |> partition_by_dest)

let t3 _ =
  session_test_x
    ([
       (`ToServer, "/connect/88/");
       (`ToClient, "/ack/88/0/");
       (`ToServer, "/data/88/0/foo\\/bar\nbar\\\\baz\n/");
       (`ToClient, "/ack/88/16/");
       (`ToClient, "/data/88/0/rab\\/oof\n/");
       (`ToServer, "/ack/88/8/");
       (`ToClient, "/data/88/8/zab\\\\rab\n/");
       (`ToServer, "/ack/88/16/");
     ]
    |> partition_by_dest)

let () = t2 ()
let () = t3 ()
