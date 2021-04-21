(* 5. Functions and Infinite Data *)

type electricity = Watts of float

type light = Lumens of float

let show_light = function Lumens lumens -> "LIGHT = " ^ (string_of_float lumens) ^ "lumens!"

let lamp lumens_per_watt = function
  Watts watts ->
    Lumens (watts *. lumens_per_watt)

let tungsten_incandescent_lamp = lamp 12.5

let halogen_lamp = lamp 24.0

let fluorescent_lamp = lamp 75.0

let led_lamp = lamp 100.0

let metal_halide_lamp = lamp 75.0

type 'a seq =
  | Nil
  | Cons of 'a * (unit -> 'a seq)
[@@deriving show]

let head = function
  | Cons (x, _) -> Some x
  | Nil -> None

let tail = function
  | Cons (_, xf) -> xf ()
  | Nil -> Nil

let consq = function
  | None, xq -> xq
  | Some x, xq -> Cons(x, fun () -> xq)

type int_seq = int seq [@@deriving show]

let rec from k = Cons (k, fun () -> from (k + 1))

(* Returns the next n elements *)
let rec takeq = function
  | (0, _xq) -> []
  | (_n, Nil) -> []
  | (n, Cons (x, xf)) -> x :: takeq ((n - 1), xf ())

type 'a seqnode =
  | NilNode
  | ConsNode of ('a * 'a lseq)
and 'a lseq =
  | Seq of (unit -> 'a seqnode)

let rec lseq_from (k: int) : int lseq =
  Seq (fun () -> ConsNode (k, lseq_from (k + 1)))

let rec lseq_takeq = function
  | (0, _xq) -> []
  | (n, Seq xq) -> node_takeq (n, xq ())
and node_takeq = function
  | (0, _xq) -> [] (* does this case ever get hit? *)
  | (_n, NilNode) -> []
  | (n, ConsNode (x, xq)) -> x :: lseq_takeq (n - 1, xq)

let rec squares = function
  | Nil -> Nil
  | Cons (x, xf) -> Cons (x * x, fun () -> squares (xf ()))

let rec addq = function
  | Cons (x, xf), Cons (y, yf) -> Cons (x + y, fun () -> addq (xf (), yf ()))
  | _ -> Nil

let rec appendq = function
  | Cons (x, xf), y -> Cons (x, fun () -> appendq (xf (), y))
  | x, Cons (y, yf) -> Cons (y, fun () -> appendq (x, yf ()))
  | Nil, Nil -> Nil