module ArrayOps : Array_sig.ARRAYOPS = struct
  exception E

  module ArrayTree = Tree
  
  open Tree

  let rec sub = function
    | Lf, _ -> raise E
    | Br (v, t1, t2), k ->
        if k = 1 then v
        else if k mod 2 = 0 then sub (t1, k / 2)
        else sub (t2, k / 2)

  let rec update = function
    | Lf, k, w ->
      if k = 1 then Br (w, Lf, Lf)
      else raise E
    | Br (v, t1, t2), k, w ->
      if k = 1 then Br (w, t1, t2)
      else if (k mod 2) = 0 then
        Br (v, update (t1, k / 2, w), t2)
      else
        Br (v, t1, update (t2, k / 2, w))

  let rec hirem = function
    | Lf, _n -> raise E
    | Br (v, t1, t2), n ->
      if n = 1 then Lf
      else if (n mod 2) = 0 then
        Br (v, hirem (t1, n / 2), t2)
      else
        Br (v, t1, hirem (t2, n / 2))

  let hiext ((ta, n), v) = update (ta, n + 1, v), n + 1

end

module Array : Array_sig.ARRAY = struct
  type 'a t = A of 'a ArrayOps.ArrayTree.t * int

  exception HiRem
  exception Update
  exception Sub

  let empty = A (ArrayOps.ArrayTree.Lf, 0)

  let sub (A (t, n)) ~index =
    if 1 <= index && index <= n then ArrayOps.sub (t, index)
    else raise Sub

  let update (A (t, n)) ~index ~value =
    if 1 <= index && index <= n then A (ArrayOps.update (t, index, value), n)
    else raise Update

  let hi_ext (A (t, n)) ~value = A (ArrayOps.update (t, n + 1, value), n + 1)

  let hi_rem (A (t, n)) =
    if n > 0 then A (ArrayOps.hirem (t, n), n - 1)
    else raise HiRem
end