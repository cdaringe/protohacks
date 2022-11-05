let get_or_default d = function None -> d | Some x -> x
let get_def def = function None -> def () | Some x -> x
let get_or_fail msg = function None -> failwith msg | Some x -> x
