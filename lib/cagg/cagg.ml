module Mean (N : Cnum.Num) = struct
  type 'a t = { count : N.t; total : N.t }

  open N

  let init _ = { count = zero; total = zero }
  let acc t v = { count = add one t.count; total = add v t.total }
  let agg t = if t.count = zero then zero else div t.total t.count
end
