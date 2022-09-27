let init _ = ref 0
let _inc = incr

let incr t =
  _inc t;
  !t
