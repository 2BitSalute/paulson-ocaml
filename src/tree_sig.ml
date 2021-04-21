(* `module type` is the equivalent of SML's `signature`; perhaps, SML's keyword is more clear *)
module type TREE = sig
  (* Must this type be concrete? *)
  type 'a t = Lf | Br of 'a * 'a t * 'a t

  val count : 'a t -> int

  val depth : 'a t -> int

  val reflect : 'a t -> 'a t
end