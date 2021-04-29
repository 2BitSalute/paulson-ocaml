module type Keyword_sig = sig
  val alphas : string list
  val symbols : string list
end

module Keyword : sig
  val alphas : string list
  val symbols : string list
end

module Make : functor (K : Keyword_sig) (B : Basic_sig.S) -> sig
  type token =
    | Id of string | Key of string

  val scan : 'a -> 'b
end
