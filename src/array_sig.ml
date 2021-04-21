(* In OCaml, as in SML, an empty module signature (`sig end`) is valid *)
module type EmptyModule = sig end

module type ARRAYOPS = sig
  (* I guess, this works the same as in SML. Is ArrayTree just an alias for TREE? *)
  (* Also, again, MUST we make these types concrete here? *)
  (* The book says something about how, without this next line, ARRAYOPS would depend on
      the type already declared, but... doesn't it, still? *)
  module ArrayTree : Tree_sig.TREE

  exception E

  (* I.e., array[index] *)
  val sub : 'a ArrayTree.t * int -> 'a

  (* I.e., array[index] = val *)
  val update : 'a ArrayTree.t * int * 'a -> 'a ArrayTree.t

  (* A mystetious function that shrinks the array by one element (pointed to by the index, I presume).
      Should not have skipped Chapter 4's functional arrays, perhaps!
      NOTE: `hirem` stands for hi + remove; there is also an hiext, for hi + extend
  *)
  val hirem : 'a ArrayTree.t * int -> 'a ArrayTree.t

  (* Takes the array and its upper bound, and a new value, inserts the value, then returns the new array and the new upper bound.
      NOTE: what a dreadful API...
  *)
  val hiext : ('a ArrayTree.t * int) * 'a -> 'a ArrayTree.t * int
end

module type ARRAY = sig
  (* Note from the book on the difference between `type` and `datatype`: `datatype` creates a new type distinct from
      all others, whereas `type` is a true alias *)
  type 'a t

  exception Sub
  exception Update
  exception HiRem

  val empty : 'a t

  val sub : 'a t -> index:int -> 'a

  val update : 'a t -> index:int -> value:'a -> 'a t

  val hi_ext : 'a t -> value:'a -> 'a t

  val hi_rem : 'a t -> 'a t
end