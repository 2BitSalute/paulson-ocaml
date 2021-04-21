exception Qerror

(* 7.1  Rpresenting queues as lists *)
module Section_7_1 = struct
  (* NOTE: in SML, the `type` keyword is used to alias an existing type,
      while the `datatype` keyword is used to define new types *)
  type 'a queue = 'a list

  let enqueue q x = q @ [x]

  let dequque = function
    | _x :: q -> q
    | [] -> raise Qerror
end

(* 7.2  Representing queues as a new datatype *)
module Section_7_2 = struct
  type 'a queue =
    | Empty
    | Enqueue of 'a queue * 'a

  let rec qhd = function
    | Enqueue (Empty, x) -> x
    | Enqueue (q, _x) -> qhd q
    | Empty -> raise Qerror

  let rec dequeue = function
    | Enqueue (Empty, _x) -> Empty
    | Enqueue (q, x) -> Enqueue (dequeue q, x)
    | Empty -> raise Qerror
end

(* 7.3  Representing queues as pairs of lists *)
module Section_7_3 = struct
  (* W. H. Burton (1982). An efficient functional representation of FIFO queues. /Information Processing Letters/, 17:471-522 *)
  type 'a queue = Queue of 'a list * 'a list

  (* The normal form of the queue is where its heads list is not empty, unless the tails list is also empty *)
  let norm = function
    | Queue ([], tails) -> Queue (List.rev tails, [])
    | q -> q

  let enqueue (Queue (heads, tails), x) =
    norm (Queue (heads, x :: tails))

  let dequeue = function
    | Queue (_x :: heads, tails) -> norm (Queue (heads, tails))
    | Queue ([], _) -> raise Qerror
end

(* 7.4  A structure for queues as lists *)
module Queue1 = struct
  type 'a t = 'a list

  exception E

  let empty = []

  let enq (q, x) = q @ [x]

  let null = function
    | [] -> true
    | _ -> false

  let hd = function
    | x :: _ -> x
    | [] -> raise E

  (* let deq q =
    match q with
    | _ :: q -> q
    | [] -> raise E *)

  let deq = function
    | _ :: q -> q
    | [] ->
      raise E

  (*
    val deq _ :: q  = q
        deq []      = raise E
    *)
end

(* 7.5  A structure of queues as a new datatype *)
module Queue2 = struct
  type 'a t =
    | Empty
    | Enq of 'a t * 'a

  exception E

  let null = function
    | Empty -> true
    | _ -> false

  let rec hd = function
    | Enq (Empty, x) -> x
    | Enq (q, _x) -> hd q
    | Empty -> raise E

  let rec deq = function
    | Enq (Empty, _) -> Empty
    | Enq (q, x) -> Enq (deq q, x)
    | Empty -> raise E
  
  (* In SML, it is legal to use names starting with lowercase letters as constructors.
      Therefore, in the book, there is no `enq` function because that's the name of
      one of t's constructors. Same goes for `empty` *)
  let enq (q, x) = Enq (q, x)

  let empty = Empty
end

(* Demonstrating the fact that the structure of Queue2 is exposed to the outside *)
let last = function Queue2.Enq (_q, x) -> x | Empty -> raise Queue2.E

(* OpaqueQueue2 is not only an alias for Queue2; it is also hiding its internal representation
    because it asserts its type as the abstract QUEUE.
    NOTE: it seems, in SML, OpaqueQueue2 is compatible with Queue2, but in OCaml, they are not. *)
module OpaqueQueue2 : Queue_sig.QUEUE = Queue2

(* 7.6  A structure for queues as pairs of lists *)
module Queue3 : Queue_sig.QUEUE = struct
  type 'a t = Queue of 'a list * 'a list

  exception E

  let empty = Queue ([], [])

  (* let norm (q : 'a t) : 'a t = 
    match q with
    | Queue ([], tails) -> Queue(List.rev tails, [])
    | q -> q *)

  let norm = function
    | Queue ([], tails) -> Queue(List.rev tails, [])
    | q -> q

  (* Could be properly called is_empty *)
  let null = function
    | Queue ([], []) -> true
    | _ -> false

  let hd = function
    | Queue (x :: _heads, _tails) -> x
    | Queue ([], _) -> raise E

  let enq (Queue (heads, tails), x) =
    norm (Queue (heads, x :: tails))

  let deq = function
    | Queue (_x :: heads, tails) -> norm (Queue (heads, tails))
    | _ -> raise E
end

(* 7.7  The `open` declaration *)
let rec qtake (i, q) =
  let open Queue3 in
  if null q || i <= 0 then []
  else (hd q) :: qtake (i - 1, deq q)
