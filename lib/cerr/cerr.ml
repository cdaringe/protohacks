let exn_string e =
  let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
  Fmt.str "%s%s" msg stack
