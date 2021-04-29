module type Keyword_sig = sig
  val alphas : string list
  val symbols : string list
end

module Keyword : Keyword_sig = struct
  let alphas = []

  let symbols = []
end

module Make (K : Keyword_sig) (B : Basic_sig.S) = struct
  type token =
    | Id of string
    | Key of string

  let scan s =
    ignore s;
    failwith "Not implemented"
end

(* module S = Lexer (Keyword) (Basic) *)