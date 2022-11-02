(* # Solution
   - Each socket gets a handler, H
   - H may read freely from its socket
   - H may never write to its socket
   - H may request writes to its own socket and peer sockets
   - A single writer writes to all sockets concurrently, in order
*)

open Effect
open Effect.Deep

let log = Eio.traceln
let create_msg name body = `Msg (name, body)

let create_peer_msgs ~peer_names body =
  List.map (fun member_name -> create_msg member_name body) peer_names

module Handshake = struct
  type _ Effect.t += Greet : unit Effect.t
  type _ Effect.t += Identify : string Effect.t

  let verify_identity id =
    let is_alpha = Cchar.is_alphanum_str id in
    let is_empty = String.length id = 0 in
    if is_alpha && not is_empty then Ok id
    else Error (Printf.sprintf "invalid name: '%s'" id)

  module Handlers = struct
    open Server

    let run ~client:(flow, _) (type a) (e : a Effect.t) =
      match e with
      | Greet ->
          Some
            (fun (k : (a, _) continuation) ->
              send_line ~flow "Welcome to budgetchat! What shall I call you?";
              continue k ())
      | Identify -> Some (fun k -> continue k (Server.read_line_term_char flow))
      | _ -> None
  end
end

module Pool = struct
  include Map.Make (String)

  type _ Effect.t += Announce : string -> unit Effect.t
  type _ Effect.t += Broadcast : string -> unit Effect.t
  type _ Effect.t += Disconnect : string -> unit Effect.t
  type _ Effect.t += Join : string -> unit Effect.t

  let keys ?(filter = fun _ -> true) t =
    fold
      (fun k _v acc -> match filter k with true -> k :: acc | _ -> acc)
      t []

  let keys_without k = keys ~filter:(fun k' -> k != k')

  module Handlers = struct
    open Server

    let run ~send ~pool ~flow (type a) (e : a Effect.t) =
      match e with
      | Announce name ->
          Some
            (fun (k : (a, _) continuation) ->
              let peer_names = keys_without name !pool in
              let body = String.concat ", " peer_names in
              let current_peers_msg = "* The room contains: " ^ body in
              send (create_msg name current_peers_msg);
              let enter_msg = "* " ^ name ^ " has entered the room" in
              let peer_msgs = create_peer_msgs ~peer_names enter_msg in
              List.iter send peer_msgs;
              continue k ())
      | Broadcast name ->
          Some
            (fun k ->
              let on_line msg =
                let peer_names = keys_without name !pool in
                List.iter send
                  (create_peer_msgs ~peer_names ("[" ^ name ^ "] " ^ msg))
              in
              (try read_lines ~on_line ~flow
               with exn -> log "%s" (Printexc.to_string exn));
              continue k ())
      | Disconnect name ->
          Some
            (fun k ->
              pool := remove name !pool;
              let peer_names = keys_without name !pool in
              let body = "* " ^ name ^ " has left the room" in
              List.iter send (create_peer_msgs ~peer_names body);
              continue k ())
      | Join name ->
          Some
            (fun k ->
              pool := add name flow !pool;
              continue k ())
      | _ -> None
  end
end

let chat id =
  perform Handshake.Greet;
  let name = String.trim @@ perform Handshake.Identify in
  match Handshake.verify_identity name with
  | Error msg -> log "%i invalid identity %s" id msg
  | Ok name ->
      let participate _ =
        perform (Pool.Join name);
        perform (Pool.Announce name);
        perform (Pool.Broadcast name)
      in
      let finally _ = perform (Pool.Disconnect name) in
      Fun.protect ~finally participate

let connection_count = Counter.init ()

let handle_socket ~enqueue ~pool flow addr =
  let id = Counter.incr connection_count in
  match_with chat id
    {
      retc = (Cfun.tap @@ fun _ -> log "%i terminated" id);
      exnc = (fun e -> log "%i failed: %a" id Fmt.exn e);
      effc =
        (fun (type a) (e : a Effect.t) ->
          List.find_map
            (fun run -> run e)
            [
              Pool.Handlers.run ~send:enqueue ~pool ~flow;
              Handshake.Handlers.run ~client:(flow, addr);
            ]);
    }

let create_msg_queue_env _ =
  let pool = ref Pool.empty in
  let q = Queue.create () in
  let is_flushing = ref false in
  let rec flush _ =
    is_flushing := true;
    match Queue.take_opt q with
    | Some (`Msg (name, msg)) ->
        (match Pool.find_opt name !pool with
        | Some flow -> (
            try Server.send_line ~flow msg with _ -> log "bummer")
        | _ -> ())
        |> flush
    | _ ->
        is_flushing := false;
        ()
  in
  let enqueue k =
    Queue.push k q;
    match !is_flushing with true -> () | false -> flush ()
  in
  (pool, enqueue)

let listen ~env ~port =
  let open Eio in
  Switch.run (fun sw ->
      let pool, enqueue = create_msg_queue_env () in
      let swo = Some sw in
      Server.listen ~swo ~env ~port ~fn:(handle_socket ~enqueue ~pool) ())
