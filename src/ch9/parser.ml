module Make (L : Types.LEXER) = struct

  exception SynError of string

  type token = L.token

  type 'a phrase = token list -> 'a * token list

  (* Phrase consisting of the keyword 'a' *)
  let ($) a = function
    | L.Key b :: tokens ->
      if a = b then
        (a, tokens)
      else
        raise (SynError a)
    | _ ->
      raise (SynError "Symbol expected")

  let id = function
    | L.Id a :: tokens -> (a, tokens)
    | _ -> raise (SynError "Identifier expected")

  let map
      (ph : ('a -> 'b * 'c))
      (f : ('b -> 'd))
      (tokens : 'a)
    : 'd * 'c =
    let ((x : 'b), (tokens : 'c)) = ph tokens in
    ((f x), tokens)

  let (>>) = map

  let alt ph1 ph2 tokens =
    try
      ph1 tokens
    with
    | SynError _ -> ph2 tokens

  let (||) = alt

  let cons
      (ph1 : 'a phrase)
      (ph2 : 'b phrase)
      (tokens : token list)
    : ('a * 'b) * token list =
    let ((x : 'a), tokens) = ph1 tokens in
    let ((y : 'b), (tokens : token list)) = ph2 tokens in
    ((x, y), tokens)

  let (--) = cons

  let empty (tokens : token list) : 'a list * token list = ([], tokens)

  let infixes
      (ph : 'a phrase)
      (precedence_of : string -> int)
      (apply : string -> 'a -> 'a -> 'a)
    : 'a phrase =
    (* Parses tokens into a series of phrases separated by operators of `precedence` or above *)
    let rec over (precendence : int) tokens =
      next precendence (ph tokens)
    (* `x` is the meaning of the preceding phrase *)
    and next (precedence : int) (x, tokens) =
      (* Do nothing unless the next token is an operator of `precedene` or above *)
      match tokens with
      | (L.Key key :: tokens) ->
        if (precedence_of key) < precedence then
          (x, L.Key key :: tokens)
        else
          let p = over (precedence_of key) in
          next precedence ((p >> apply key x) tokens)
      | tokens -> (x, tokens)
    in
    over 0

  let rec repeat
      (ph : 'a phrase)
      (tokens : token list)
    : 'a list * token list =
    (*
      In the book, the list construction was achieved with just the `::` operator,
      which is not an operator in OCaml, it's a List constructor (and the only infix constructor in existence).
      See https://stackoverflow.com/questions/13844151/in-ocaml-why-is-the-list-constructor-not-an-operator
    *)
    tokens |> (ph -- (repeat ph) >> (fun (el, l) -> el :: l) || empty)

  let reader (ph : 'a phrase) (a : string) : 'a =
    match ph (L.scan a) with
    | (x, []) -> x
    | (_, _ :: _) -> raise (SynError "Extra characters in phrase")
end