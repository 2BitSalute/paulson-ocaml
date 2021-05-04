module type BASIC = sig
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

module type KEYWORD = sig
  val alphas : string list
  val symbols : string list
end

(* The signature of lexer modules *)
module type LEXER = sig
  type token =
    | Id of string | Key of string

  (* TODO: instead of a string, take a character sequence *)
  (* TODO: instead of returning a token list, return a sequence *)
  (* TODO: for both, see lazy.ml; e.g., return `token Lazy.seq` *)
  val scan : string -> token list
end

(* The functor that creates lexers when parametrized by a keyword module *)
module type MakeLexer = functor (K : KEYWORD) (B : BASIC) -> LEXER

(* module type PARSER = sig
   (* NOTE: I chose not to throw exceptions in favor of using the result type *)
   type error

   type token

   val reader :
    (token list -> ('a * 'b list, error) result) ->
    string ->
    ('a, error) result

   (* Consecutive phrases *)
   val (--) :
    ('a -> ('b * 'c, error) result) ->
    ('c -> ('d * 'e, error) result) ->
    'a ->
    (('b * 'd) * 'e, error) result

   val cons :
    ('a -> ('b * 'c, error) result) ->
    ('c -> ('d * 'e, error) result) ->
    'a ->
    (('b * 'd) * 'e, error) result

   (* Modifying the meaning *)
   val (>>) :
    ('a -> ('b * 'c, error) result) ->
    ('b -> 'd) ->
    'a ->
    ('d * 'c, error) result

   val map :
    ('a -> ('b * 'c, error) result) ->
    ('b -> 'd) ->
    'a ->
    ('d * 'c, error) result

   (* Alternative phrases *)
   val (||) :
    ('a -> ('b, error) result) ->
    ('a -> ('b, error) result) ->
    'a ->
    ('b, error) result

   val alt :
    ('a -> ('b, error) result) ->
    ('a -> ('b, error) result) ->
    'a ->
    ('b, error) result

   (* Key token parser function *)
   val ($) :
    string ->
    token list ->
    (string * token list, error) result

   (* Id token parser function *)
   val id :
    token list ->
    (string * token list, error) result

   val empty : 'a -> ('b list * 'a, error) result

   val infixes :
    (token list -> 'a * token list) * (string -> int) * (string -> 'a -> 'a -> 'a) ->
    token list ->
    'a * token list

   val repeat :
    ('a -> ('b * 'a, error) result) ->
    'a ->
    'b list * 'a
   end

   module type MakeParser = functor (L : LEXER) -> PARSER *)

module type PARSER = sig
  exception SynError of string

  type token

  (* The book points out that in SML signatures, type aliases/abbreviations are not allowed,
      but since we're in OCaml, we can use them *)
  type 'a phrase = token list -> 'a * token list

  val reader :
    'a phrase ->
    string ->
    'a

  (* Consecutive phrases *)
  val (--) :
    'a phrase ->
    'b phrase ->
    token list ->
    ('a * 'b) * token list

  val cons :
    'a phrase ->
    'b phrase ->
    token list ->
    ('a * 'b) * token list

  (* Modifying the meaning *)
  val (>>) :
    ('a -> 'b * 'c) ->
    ('b -> 'd) ->
    'a ->
    'd * 'c

  val map :
    ('a -> 'b * 'c) ->
    ('b -> 'd) ->
    'a ->
    'd * 'c

  (* Alternative phrases *)
  val (||) : 'a phrase -> 'a phrase -> 'a phrase

  val alt : 'a phrase -> 'a phrase -> 'a phrase

  (* Key token parser function *)
  val ($) : string -> string phrase

  (* Id token parser function *)
  val id : string phrase

  val empty : token list -> 'a list * token list

  val infixes :
    'a phrase ->
    (string -> int) ->
    (string -> 'a -> 'a -> 'a) ->
    'a phrase

  val repeat :
    ('a phrase) ->
    token list ->
    'a list * token list
end

module type MakeParser = functor (L : LEXER) -> PARSER