module Make (K : Types.Keyword) (B : Types.Basic_sig) = struct
  open B

  type token =
    | Id of string
    | Key of string

  let is_letter_or_digit c =
    "A" <= c &&
    "Z" >= c ||
    "a" <= c &&
    "z" >= c ||
    "0" <= c &&
    "9" >= c

  let explode s = Core.String.to_list s |> List.map (fun c -> Char.escaped c)

  let specials = explode "!@#$%^&*()+-=[]:\"|;',./?\\`_~<>"


  type alpha_res = {
    id: string;
    alpha_remainder: string list;
  }

  let rec alphanum id = function
    | [] -> { id; alpha_remainder = [] }
    | (c :: cs) ->
      if is_letter_or_digit c
      then alphanum (id ^ c) cs
      else { id; alpha_remainder = c :: cs }

  let tokenof (a : string) =
    if a /? K.alphas then Key a else Id a

  type symbolic_res = {
    sy: string;
    sy_remainder: string list;
  }

  let rec symbolic sy = function
    | [] -> { sy; sy_remainder = [] }
    | (c :: cs) ->
      if sy /? K.symbols || not (c /? specials)
      then { sy; sy_remainder = c :: cs }
      else symbolic (sy ^ c) cs

  let rec scanning tokens = function
    | [] -> List.rev tokens
    | c :: cs ->
      if is_letter_or_digit c then (* Identifier or keyword *)
        begin
          let { id; alpha_remainder = remainder } = alphanum c cs in
          scanning (tokenof id :: tokens) remainder
        end
      else if c /? specials then (* Symbolic keyword *)
        begin
          let {sy ; sy_remainder = remainder } = symbolic c cs in
          scanning (Key sy :: tokens) remainder
        end
      else (* Skips spaces, line breaks, strange characters *)
        scanning tokens cs

  let scan s =
    scanning [] (explode s)

end