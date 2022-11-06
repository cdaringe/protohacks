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
    let next_pings = (camera.mile, timestamp) :: pings |> Clist.sort_by_snd in
    traceln "pings (road %i) (plate: %s): %s" camera.road vehicle.plate
      (show_pings next_pings);
    Hashtbl.replace vehicle.pings_by_road camera.road next_pings

  let has_ticket_on t day = Hashtbl.mem t.tickets_by_day day
  let mark_ticket_on t day = Hashtbl.replace t.tickets_by_day day true

  let find_violation ~pings ~limit =
    let check_violation (b_mile, b_ts) (a_mile, a_ts) =
      let seconds = b_ts - a_ts in
      let miles = b_mile - a_mile in
      (* traceln "(distance: %f, duration: %f [%f])" miles seconds hrs; *)
      match (miles, seconds) with
      | 0, _ -> None
      | _, 0 -> failwith "invalid duration"
      | _ -> (
          (* miles / (seconds / s_per_h) ==> a / (x/y) ==> xb / y *)
          (* let speed_100x_m_p_h = (100 * miles) / (seconds / 3600) in *)
          let speed_100x_m_p_h = 100 * miles * 3600 / seconds in
          let limit_100x_m_p_h = 100 * limit in
          let abs_speed_100x = Cnum.abs_i speed_100x_m_p_h in
          match abs_speed_100x > limit_100x_m_p_h with
          | true -> Some (speed_100x_m_p_h, abs_speed_100x)
          | _ -> None)
    in
    let rec emit_violations l =
      match l with
      | [] | [ _ ] -> []
      | a :: tail -> (
          try
            let b = List.hd tail in
            let next =
              match check_violation b a with
              | Some (speed, abs_speed) -> (
                  let m1, _t1 = a in
                  let m2, _t2 = b in
                  match (m1 < m2, speed > 0) with
                  (* (m1 -> m2) *)
                  | true, true (* (m2 <- m1) *) | false, false ->
                      (a, b, abs_speed) :: emit_violations tail
                  | true, false | false, true ->
                      (b, a, abs_speed) :: emit_violations tail)
              | None -> emit_violations tail
            in
            next
          with _ -> [])
    in
    emit_violations pings
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
  let get_dispatcher_opt road = Hashtbl.find_opt dispatchers_by_road_id road

  let rec get_dispatcher ~wait ~tries road =
    match (get_dispatcher_opt road, tries) with
    | None, 0 -> failwith "could not find dispatcher"
    | None, _ ->
        wait ();
        get_dispatcher ~wait ~tries:(tries - 1) road
    | Some d, _ -> d

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
    (* traceln "getting camera ::%i" camera_id; *)
    let camera = get_camera_exn camera_id in
    Vehicle.add_observation ~timestamp:plate.timestamp ~camera vehicle;
    perform (AttemptTicketing (plate, camera))

  let add_road_client c id =
    match Hashtbl.mem monitors_by_id id with
    | false -> Hashtbl.add monitors_by_id id c
    | true -> failwith "client already present"

  let add_camera (c : camera) id =
    (* traceln "adding camera ::%i" id; *)
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
    let { road; limit; _ } = camera in
    let plate = plate'.plate in
    let v =
      Hashtbl.find_opt vehicles_by_plate plate |> function
      | Some v -> v
      | None -> failwith ("could not find vehicle " ^ plate)
    in
    let on_violation ((mile1, timestamp1), (mile2, timestamp2), speed) =
      let day1 = day_index timestamp1 in
      let day2 = day_index timestamp2 in
      let rec ticketed_between current target =
        if Vehicle.has_ticket_on v current then true
        else if current < target then ticketed_between (current + 1) target
        else false
      in
      let is_already_ticketed = ticketed_between day1 day2 in
      traceln "%s ticketed on (%i - %i): %b" v.plate day1 day2
        is_already_ticketed;
      if is_already_ticketed then ()
      else
        let rec mark_between current target =
          Vehicle.mark_ticket_on v current;
          if current < target then mark_between (current + 1) target else ()
        in
        mark_between day1 day2;
        (* traceln "issue ticket to %s on road %i" plate road; *)
        (* let int_speed = int_of_float @@ (speed *. 100.) in *)
        let t =
          {
            plate;
            road = camera.road;
            mile1;
            timestamp1;
            mile2;
            timestamp2;
            speed;
          }
        in
        traceln "Sending ticket: %s" (show_ticket t);
        perform (IssueTicket t)
    in
    let pings = Hashtbl.find v.pings_by_road road in
    let violations = Vehicle.find_violation ~pings ~limit in
    List.iter on_violation violations
end

let install_heartbeat ~hb ~sw ~env ~send socklog msg =
  match !hb with
  | Some _ -> failwith "hb already exists"
  | None ->
      let interval = Float.of_int msg.interval /. 10. in
      if interval <= 0. then ()
      else
        hb :=
          Some
            (Fiber.fork_daemon ~sw (fun _ ->
                 let clock = Eio.Stdenv.clock env in
                 while true do
                   socklog "heartbeat";
                   send Msg.Heartbeat;
                   Time.sleep clock interval
                 done;
                 `Stop_daemon))

let socket_id = ref 0

let get_id _ =
  incr socket_id;
  !socket_id

let rec with_effects ?(root = false) ~env ~send fn =
  let effc (type a) (e : a Effect.t) =
    match e with
    | AttemptTicketing t ->
        Some
          (fun (k : (a, _) continuation) ->
            with_effects ~env ~send (fun _ -> Roads.attempt_ticket t);
            continue k ())
    | IssueTicket t ->
        Some
          (fun (k : (a, _) continuation) ->
            let wait _ =
              traceln "waiting for dispatcher";
              let clock = Eio.Stdenv.clock env in
              Time.sleep clock 0.5
            in
            let dispatch_flow = Roads.get_dispatcher ~wait ~tries:10 t.road in
            Msg.Server.send ~flow:dispatch_flow (Msg.Ticket t);
            continue k ())
    | _ -> None
  in
  match_with fn ()
    {
      retc = (Cfun.tap @@ fun _ -> () (* traceln "retc" *));
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
  let socklog m = traceln "[%i] %s" id m in
  let open Msg.Server in
  let read = Eio.Buf_read.of_flow flow ~initial_size:100 ~max_size:1_000_000 in
  let send = Msg.Server.send ~flow in
  let hb = ref None in
  let i = ref 0 in
  let rec process_msgs _ =
    incr i;
    try
      let msg = parse read in
      socklog @@ Fmt.str "{%i} %s" !i (Msg.show_client_msg msg);
      (match msg with
      | IAmCamera t -> Roads.add_camera t id
      | IAmDispatcher t -> Roads.add_dispatcher t flow id
      | Plate t -> Roads.add_plate t id
      | WantHeartbeat t -> install_heartbeat ~hb ~sw ~env ~send socklog t);
      process_msgs ()
    with End_of_file -> ()
  in
  with_effects ~env ~send ~root:true process_msgs
(* socklog "pending heartbeat"; *)
(* Promise.await_exn (Option.get !hb); *)
(* try Promise.await_exn (Option.get !hb) *)
(* with e -> *)
(* socklog @@ Fmt.str "%a" Fmt.exn_backtrace (e, Printexc.get_raw_backtrace ()); *)
(* socklog "socket closing" *)

let listen ~env ~port =
  let handle_socket_in_isolated_switch a b =
    Switch.run (fun sw -> handle_socket ~sw ~env a b)
  in
  Server.listen ~env ~port ~fn:handle_socket_in_isolated_switch ()
