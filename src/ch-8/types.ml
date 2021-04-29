module type Basic_sig = sig
  val minl : int list -> int option

  val maxl : int list -> int option

  val take : count:int -> 'a list -> 'a list

  val drop : count:int -> 'a list -> 'a list

  val nth : index:int -> 'a list -> 'a option

  val mem : 'a -> 'a list -> bool

  (* NOTE: this operator cannot be, e.g., `???` because the first character determines whether
     it's an infix or a prefix operator, and `?` indicates a prefix operator *)
  val (/?) : 'a -> 'a list -> bool

  val newmem : 'a -> 'a list -> 'a list

  val lookup : key:'a -> ('a * 'b) list -> 'b option

  val filter : ('a -> bool) -> 'a list -> 'a list

  val exists : ('a -> bool) -> 'a list -> bool

  val forall : ('a -> bool) -> 'a list -> bool

  val foldleft : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

  val foldright : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
end

module type Keyword = sig
  val alphas : string list
  val symbols : string list
end

module type MakeLexer = functor (K : Keyword) (B : Basic_sig) -> sig
  type token =
    | Id of string | Key of string

  (* TODO: instead of a string, take a character sequence *)
  (* TODO: instead of returning a token list, return a sequence *)
  (* TODO: for both, see lazy.ml; e.g., return `token Lazy.seq` *)
  val scan : string -> token list
end