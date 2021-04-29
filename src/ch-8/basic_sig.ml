module type S = sig
  val minl : int list -> int option

  val maxl : int list -> int option

  val take : count:int -> 'a list -> 'a list

  val drop : count:int -> 'a list -> 'a list

  val nth : index:int -> 'a list -> 'a option

  val mem : 'a -> 'a list -> bool

  val newmem : 'a -> 'a list -> 'a list

  val lookup : key:'a -> ('a * 'b) list -> 'b option

  val filter : ('a -> bool) -> 'a list -> 'a list

  val exists : ('a -> bool) -> 'a list -> bool

  val forall : ('a -> bool) -> 'a list -> bool

  val foldleft : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

  val foldright : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
end