  (* Must this type be concrete? *)
  type 'a t = Lf | Br of 'a * 'a t * 'a t

  let rec count : 'a t -> int = function
    | Lf -> 0
    | Br (_v, t1, t2) -> 1 + count t1 + count t2

  let rec depth : 'a t -> int = function
    | Lf -> 0
    | Br (_v, t1, t2) -> 1 + max (depth t1) (depth t2)

  let rec reflect : 'a t -> 'a t = function
    | Lf -> Lf
    | Br (v, t1, t2) -> Br (v, reflect t2, reflect t1)