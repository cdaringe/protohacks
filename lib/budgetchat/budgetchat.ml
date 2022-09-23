open Effect
open Effect.Deep
module MapKStr = Map.Make (String)

module Pool = struct
  type t
  type _ Effect.t += Connect : string -> unit Effect.t
  type _ Effect.t += Announce : string -> unit Effect.t
  type _ Effect.t += Broadcast : string -> unit Effect.t
  type _ Effect.t += Disconnect : string -> unit Effect.t

  let announce m = perform (Announce m)
  let broadcast prefix = perform (Broadcast prefix)
  let join name = perform (Connect name)
  let leave name = perform (Disconnect name)
end

module Handshake = struct
  type _ Effect.t += Greet : string -> unit Effect.t
  type _ Effect.t += Identify : string Effect.t

  let init _ = perform (Greet "guten tag")

  let verify_identity () =
    let id = perform Identify in
    if Cchar.is_alphanum_str id && String.length id > 0 then id
    else raise (Failure ("invalid: " ^ id))
end

let handle_stream _ =
  let name = Handshake.(init () |> verify_identity) in
  let open Pool in
  join name;
  announce name;
  broadcast ("[" ^ name ^ "] ");
  leave name;
  ()

let handle_effects ~pool ~flow f =
  let open Server in
  let name = ref "" in
  let send_to_others msg =
    let send_ignore_me name' flow =
      if !name != name' then send_line ~flow msg
    in
    MapKStr.iter send_ignore_me !pool
  in

  try_with f ()
    {
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Handshake.Greet msg ->
              Some
                (fun (k : (a, _) continuation) ->
                  send_line ~flow msg;
                  continue k ())
          | Handshake.Identify ->
              Some
                (fun k ->
                  name := read_line ();
                  continue k !name)
          | Pool.Announce name ->
              Some
                (fun k ->
                  let other_names =
                    MapKStr.fold
                      (fun k _ xs -> if k = name then xs else k :: xs)
                      !pool []
                  in
                  let others = String.concat ", " other_names in
                  send_line ~flow ("* The room contains: " ^ others);
                  send_to_others ("* " ^ name ^ " has entered the room");
                  continue k ())
          | Pool.Broadcast prefix ->
              Some
                (fun k ->
                  let on_line msg = send_to_others (prefix ^ msg) in
                  read_lines ~on_line ~flow;
                  continue k ())
          | Pool.Connect name ->
              Some
                (fun k ->
                  pool := MapKStr.add name flow !pool;
                  continue k ())
          | _ -> None);
    }

let listen ~env ~port =
  let pool = ref MapKStr.empty in
  let fn flow _ = handle_effects ~pool ~flow handle_stream in
  Server.listen ~env ~port ~fn
