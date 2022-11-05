open Effect
open Effect.Deep
open Eio
module IntMap = Map.Make (Int)

type camera = { road : int; mile : int; limit : int } [@@deriving show]
type dispatcher = { roads : int list } [@@deriving show]
type heartbeat_req = { interval : int } [@@deriving show]
type camera_markers = (int * camera) list [@@deriving show]
type pings = (int * int) list [@@deriving show]

type plate = {
  plate : string;
  timestamp : int; (* int32 may be more appropriate? *)
}
[@@deriving show]

type monitor = Cam of camera | Dis of dispatcher [@@deriving show]
type ticket_check = plate * camera [@@deriving show]

type ticket = {
  plate : string;
  road : int;
  mile1 : int;
  timestamp1 : int;
  mile2 : int;
  timestamp2 : int;
  speed : int;
}
[@@deriving show]

type _ Effect.t += AttemptTicketing : ticket_check -> unit Effect.t
type _ Effect.t += IssueTicket : ticket -> unit Effect.t

module Vehicle = struct
  type t = {
    plate : string;
    pings_by_road : (int, pings) Hashtbl.t;
    tickets_by_day : (int, bool) Hashtbl.t;
  }

  let create plate _ =
    {
      plate;
      pings_by_road = Hashtbl.create 10;
      tickets_by_day = Hashtbl.create 10;
    }

  let add_observation ~timestamp ~(camera : camera) vehicle =
    let pings =
      Hashtbl.find_opt vehicle.pings_by_road camera.road
      |> Copt.get_or_default []
    in
    let next_pings = (camera.mile, timestamp) :: pings |> Clist.sort_by_fst in
    traceln "pings (road %i) (plate: %s): %s" camera.road vehicle.plate
      (show_pings next_pings);
    Hashtbl.replace vehicle.pings_by_road camera.road next_pings

  let has_ticket_on t day = Hashtbl.mem t.tickets_by_day day
  let mark_ticket_on t day = Hashtbl.replace t.tickets_by_day day true

  let find_violation ~road ~limit t =
    let pings = Hashtbl.find t.pings_by_road road in
    let f = float_of_int in
    let check_violation (b_mile, b_ts) (a_mile, a_ts) =
      let seconds = f (b_ts - a_ts) in
      let miles = f (b_mile - a_mile) in
      let hrs = seconds /. 3600.0 in
      traceln "(distance: %f, duration: %f [%f])" miles seconds hrs;
      match (miles, seconds) with
      | 0., _ -> None
      | _, 0. -> failwith "invalid duration"
      | _ -> (
          let speed = miles /. hrs in
          traceln "%f > %i ?" speed limit;
          match speed > float_of_int limit with true -> Some speed | _ -> None)
    in
    let rec find_violation' l =
      match l with
      | [] | [ _ ] -> None
      | a :: tail -> (
          try
            let b = List.hd tail in
            match check_violation b a with
            | Some speed -> Some (a, b, speed)
            | None -> find_violation' tail
          with _ -> None)
    in
    find_violation' pings
end

module Msg = struct
  type client_msg =
    | Plate of plate
    | WantHeartbeat of heartbeat_req
    | IAmCamera of camera
    | IAmDispatcher of dispatcher
  [@@deriving show]

  type server_msg = Error of { msg : string } | Ticket of ticket | Heartbeat
  [@@deriving show]

  module Deserialize = struct
    open Eio.Buf_read.Syntax

    let u8 =
      let+ c = Buf_read.any_char in
      Char.code c

    let u16 =
      let+ str_of_bytes = Buf_read.take 2 in
      String.get_uint16_be str_of_bytes 0

    let u32 =
      let+ str_of_bytes = Buf_read.take 4 in
      Int32.to_int @@ String.get_int32_be str_of_bytes 0

    let str =
      let* len = u8 in
      Buf_read.take len

    let parse_roads =
      let* numroads = u8 in
      let roads b = List.init numroads (fun _ -> u16 b) in
      roads
  end

  module Serialize = struct
    open Buf_write

    let str b s =
      char b (Char.chr @@ String.length s);
      string b s

    let u8 b i = uint8 b i

    let u16 b i =
      let u16_bytes = Bytes.create 2 in
      Bytes.set_uint16_be u16_bytes 0 i;
      bytes b u16_bytes

    let u32 b i =
      let u32_bytes = Bytes.create 4 in
      Bytes.set_int32_be u32_bytes 0 (Int32.of_int i);
      bytes b u32_bytes
  end

  module Server = struct
    let parse =
      let open Eio.Buf_read.Syntax in
      let open Deserialize in
      let* msg_id = u8 in
      match msg_id with
      | 0x20 ->
          let* plate = str in
          let+ timestamp = u32 in
          Plate { plate; timestamp }
      | 0x40 ->
          let+ interval = u32 in
          WantHeartbeat { interval }
      | 0x80 ->
          let* road = u16 in
          let* mile = u16 in
          let+ limit = u16 in
          IAmCamera { road; mile; limit }
      | 0x81 ->
          let+ roads = parse_roads in
          IAmDispatcher { roads }
      | _ -> failwith "invalid client msg id"

    let prepare_msg bufw =
      let open Serialize in
      function
      | Error { msg } ->
          u8 bufw 0x10;
          str bufw msg;
          ()
      | Heartbeat ->
          u8 bufw 0x41;
          ()
      | Ticket t ->
          u8 bufw 0x21;
          str bufw t.plate;
          u16 bufw t.road;
          u16 bufw t.mile1;
          u32 bufw t.timestamp1;
          u16 bufw t.mile2;
          u32 bufw t.timestamp2;
          u16 bufw t.speed;
          ()

    let send ~flow m = Buf_write.with_flow flow (fun w -> prepare_msg w m)
  end
end

let s_per_day = 24 * 60 * 60
let day_index s = s / s_per_day

module Roads = struct
  let monitors_by_id = Hashtbl.create 10
  let cameras_by_road_id = Hashtbl.create 10
  let dispatchers_by_road_id = Hashtbl.create 10
  let vehicles_by_plate = Hashtbl.create 10

  let get_camera_exn id =
    match Hashtbl.find_opt monitors_by_id id with
    | Some (Cam cam) -> cam
    | _ -> failwith ("cannot find camera " ^ string_of_int id)

  let get_or_init_vehicle (plate : plate) =
    match Hashtbl.find_opt vehicles_by_plate plate.plate with
    | None ->
        let v = Vehicle.create plate.plate () in
        Hashtbl.add vehicles_by_plate plate.plate v;
        v
    | Some v -> v

  let add_plate (plate : plate) camera_id =
    let vehicle = get_or_init_vehicle plate in
    traceln "getting camera ::%i" camera_id;
    let camera = get_camera_exn camera_id in
    Vehicle.add_observation ~timestamp:plate.timestamp ~camera vehicle;
    perform (AttemptTicketing (plate, camera))

  let add_road_client c id =
    match Hashtbl.mem monitors_by_id id with
    | false -> Hashtbl.add monitors_by_id id c
    | true -> failwith "client already present"

  let add_camera (c : camera) id =
    traceln "adding camera ::%i" id;
    add_road_client (Cam c) id;
    let cameras = try Hashtbl.find cameras_by_road_id c.road with _ -> [] in
    (* add, and sort cameras by position *)
    let next_cameras = (c.mile, c) :: cameras |> Clist.sort_by_fst in
    traceln "%s" (show_camera_markers next_cameras);
    Hashtbl.replace cameras_by_road_id c.road next_cameras

  let add_dispatcher (d : dispatcher) flow id =
    add_road_client (Dis d) id;
    let fn road_id =
      (* let existing =
           try Hashtbl.find dispatchers_by_road_id road_id with _ -> IntMap.empty
         in
         let next = IntMap.add id flow existing in *)
      Hashtbl.replace dispatchers_by_road_id road_id flow
    in
    List.iter fn d.roads

  let attempt_ticket ((plate', camera) : ticket_check) =
    traceln "attempt ticket?";
    let { road; limit; _ } = camera in
    let { plate; timestamp } = plate' in
    let v =
      Hashtbl.find_opt vehicles_by_plate plate |> function
      | Some v -> v
      | None -> failwith ("could not find vehicle " ^ plate)
    in
    let day = day_index timestamp in
    if Vehicle.has_ticket_on v day then traceln "ticket already issue"
    else traceln "finding violations,,,";
    match Vehicle.find_violation ~road ~limit v with
    | None -> ()
    | Some ((mile1, timestamp1), (mile2, timestamp2), speed) ->
        Vehicle.mark_ticket_on v day;
        traceln "issue ticket to %s on road %i" plate road;
        let int_speed = int_of_float @@ (speed *. 100.) in
        let t =
          {
            plate;
            road = camera.road;
            mile1;
            timestamp1;
            mile2;
            timestamp2;
            speed = int_speed;
          }
        in
        traceln "%s" (show_ticket t);
        perform (IssueTicket t)
end

let install_heartbeat ~hb ~sw ~env ~send msg =
  match !hb with
  | Some _ -> failwith "hb already exists"
  | None ->
      let interval = Float.of_int msg.interval /. 10. in
      if interval <= 0. then ()
      else
        hb :=
          Some
            (Fiber.fork_promise ~sw (fun _ ->
                 let clock = Eio.Stdenv.clock env in
                 while true do
                   send Msg.Heartbeat;
                   Time.sleep clock interval
                 done))

let socket_id = ref 0

let get_id _ =
  incr socket_id;
  !socket_id

let rec with_effects ?(root = false) ~send fn =
  traceln "with_effects enter";
  let effc (type a) (e : a Effect.t) =
    match e with
    | AttemptTicketing t ->
        Some
          (fun (k : (a, _) continuation) ->
            with_effects ~send (fun _ -> Roads.attempt_ticket t);
            continue k ())
    | IssueTicket t ->
        Some
          (fun (k : (a, _) continuation) ->
            let dispatch_flow =
              Hashtbl.find_opt Roads.dispatchers_by_road_id t.road
              |> Copt.get_or_fail
                   ("dispatcher for road missing: " ^ string_of_int t.road)
            in
            traceln "sending ticket to ...: %s" (show_ticket t);
            Msg.Server.send ~flow:dispatch_flow (Msg.Ticket t);
            continue k ())
    | _ -> None
  in
  match_with fn ()
    {
      retc =
        ( Cfun.tap @@ fun _ ->
          traceln "retc";
          if root then traceln "terminating" );
      exnc =
        (fun e ->
          traceln "exnc: %a" Fmt.exn e;
          if root then
            try send (Msg.Error { msg = Fmt.(to_to_string exn e) })
            with _ -> ());
      effc;
    }

let handle_socket ~sw ~env flow _ =
  let id = get_id () in
  let open Msg.Server in
  let read = Eio.Buf_read.of_flow flow ~initial_size:100 ~max_size:1_000_000 in
  let send = Msg.Server.send ~flow in
  let hb = ref None in
  let i = ref 0 in
  let rec process_msgs _ =
    incr i;
    traceln "reading msg %i" !i;
    try
      let msg = parse read in
      traceln "msg %s" (Msg.show_client_msg msg);
      (match msg with
      | IAmCamera t -> Roads.add_camera t id
      | IAmDispatcher t -> Roads.add_dispatcher t flow id
      | Plate t -> Roads.add_plate t id
      | WantHeartbeat t -> install_heartbeat ~hb ~sw ~env ~send t);
      process_msgs ()
    with End_of_file ->
      traceln "eof at %i" !i;
      ()
  in
  with_effects ~send ~root:true process_msgs;
  traceln "pending heartbeat";
  Promise.await_exn (Option.get !hb);
  traceln "handle_socket exit"

let listen ~env ~port =
  let handle_socket_in_isolated_switch a b =
    Switch.run (fun sw -> handle_socket ~sw ~env a b)
  in
  Server.listen ~env ~port ~fn:handle_socket_in_isolated_switch ()
