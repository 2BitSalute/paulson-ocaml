module type PQUEUE = sig
  module Order : Order.ORDER

  type t

  exception E

  val empty : t
  val is_empty : t -> bool
  val head : t -> Order.t
  val enq : Order.t -> t -> t
  val deq : t -> t

end

(* The implementation is based on 4.14 and 7.13 *)
(* NOTE: again, as in TableFUN, it is necessary to un-abstract the type of the element, which, in this case, is Order.t *)
module PQueueFUN (Order : Order.ORDER) (ArrayOps : Array_sig.ARRAYOPS) : PQUEUE with type Order.t = Order.t = struct
  module Order = Order

  type t = PQueue of Order.t ArrayOps.ArrayTree.t * int

  exception E

  open ArrayOps.ArrayTree

  let (<<) v1 v2 = Order.less v1 v2

  let rec upheap t n w =
    match t with
    | Lf -> Br (w, Lf, Lf)
    | Br (v, t1, t2) -> (* assume n > 1 *)
      if w << v then
        if n mod 2 = 0 then
          Br (v, upheap t1 (n / 2) w, t2)
        else
          Br (v, t1, upheap t2 (n / 2) w)
      else (* w >= v *)
        if n mod 2 = 0 then
          Br (w, upheap t1 (n / 2) v, t2)
        else
          Br (w, t1, upheap t2 (n / 2) v)

  let rec downheap t w =
    match t with
    | Lf -> raise E
    | Br (_, t1, t2) ->
      begin
        match t1 with
        | Lf -> Br (w, Lf, Lf)
        | Br (v1, _, _) ->
          begin
            match t2 with
            | Lf ->
              if v1 << w then Br (w, t1, Lf)
              else Br (v1, downheap t1 w, Lf)
            | Br (v2, _, _) ->
              if v1 << v2 then
                if v2 << w then Br (w, t1, t2)
                else Br (v2, t1, downheap t2 w)
              else if v1 << w then Br (w, t1, t2)
              else Br (v1, downheap t1 w, t2)
          end
      end
  let empty = PQueue (Lf, 0)

  let is_empty = function
    | PQueue (Br _, _) -> false
    | PQueue (Lf, _) -> true

  let head = function
    | PQueue (Br (v, _, _), _) -> v
    | PQueue (Lf, _) -> raise E

  let enq v (PQueue (t, n)) =
    let new_t = upheap t (n + 1) v in
    PQueue (new_t, n + 1)

  let deq (PQueue (t, n)) =
    if n > 1 then
      let new_t = 
        downheap
          (ArrayOps.hirem (t, n))
          (ArrayOps.sub (t, n))
      in
      PQueue (new_t, n - 1)
    else if n = 1 then empty
    else raise E
end

module PQueueInt = PQueueFUN (Order.IntOrder) (Array_ops.ArrayOps)