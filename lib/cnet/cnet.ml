open Eio

let get_ip ~env ?service url =
  match List.hd @@ Net.getaddrinfo ?service env#net url with
  | `Tcp x -> x
  | _ -> failwith "unable to get upstream ip"
