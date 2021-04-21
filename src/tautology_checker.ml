(* Propositional logic elements *)
type prop =
  | Atom of string
  | Neg of prop
  | Conj of prop * prop
  | Disj of prop * prop

(* If p -> q, then ~p | q (meaning, either NOT p is true OR q is true) *)
let implies p q = Disj (Neg p, q)

let (-->) = implies

let rich = Atom "rich"
and landed = Atom "landed"
and saintly = Atom "saintly"

(* It looks like  *)
let assumption1 = landed --> rich
let assumption2 = Neg (Conj (saintly, rich))
let conclusion = landed --> Neg saintly

let goal = Conj (assumption1, assumption2) --> conclusion

(* This syntax is the equivalent of SML's `fun show (Atom a) = a | show (Neg p) = ...` *)
let rec show_explicit = function
  | Atom a -> a
  | Neg p -> "(~" ^ show_explicit p ^ ")"
  | Conj (p, q) -> "(" ^ show_explicit p ^ " & " ^ show_explicit q ^ ")"
  | Disj (p, q) -> "(" ^ show_explicit p ^ " | " ^ show_explicit q ^ ")"

(* Exercise 4.34  Write a version of `show` that suppresses needless parentheses.
    If ~ has the highest precedence and | the lowest, then all the parentheses
    in `((~a) & b) | c` are redundant. Since & and | are associative, suppress
    parentheses in (a & b) & (c & d)
*)
let rec show_implicit prec =
  function
  | Atom a -> a
  | Neg p -> "~" ^ show_implicit 0 p
  | Conj (p, q) ->
    let p = show_implicit 1 p in
    let q = show_implicit 1 q in
    let s = p ^ " & " ^ q in
    if prec < 1 then "(" ^ s ^ ")" else s
  | Disj (p, q) -> 
    let p = show_implicit 2 p in
    let q = show_implicit 2 q in
    let s = p ^ " | " ^ q in
    if prec < 2 then "(" ^ s ^ ")" else s

let show prep = show_implicit 3 prep

(* Exercise 4.35  Write a function to evaluate a proposition using the standard
    truth tables. one argument should be a list of the true atoms; all others are
    to be assumed false.
*)

let eval_prop prop ~(true_atoms : string list) = 
  let rec aux = function
    | Atom a -> List.mem a true_atoms
    | Neg p -> not (aux p)
    | Conj (p, q) -> aux p && aux q
    | Disj (p, q) -> aux p || aux q
  in
  aux prop

(* 4.16 - Negation Normal Form
    Rewrites propositions such that negations are only applied to atoms
*)
let rec nnf = function
  | Atom a -> Atom a
  | Neg (Atom a) -> Neg (Atom a)
  | Neg (Neg p) -> nnf p
  | Neg (Conj (p, q)) -> nnf (Disj (Neg p, Neg q))
  | Neg (Disj (p, q)) -> nnf (Conj (Neg p, Neg q))
  | Conj (p, q) -> Conj (nnf p, nnf q)
  | Disj (p, q) -> Disj (nnf p, nnf q)

(* This next version of nnf avoids the needless construction of Neg propositions above in the (Neg Disj) and (Neg Conj) cases *)
(* Interesting thing about the `let rec. . .and` syntax: the `and` function
    is also recursive, and it's not valid to mark it as `rec`
*)
let rec nnfpos = function
  | Atom a -> Atom a
  | Neg p -> nnfneg p
  | Conj (p, q) -> Conj (nnfpos p, nnfpos q)
  | Disj (p, q) -> Disj (nnfpos p, nnfpos q)
and nnfneg = function
  | Atom a -> Neg (Atom a)
  | Neg p -> nnfpos p
  | Conj (p, q) -> Disj (nnfneg p, nnfneg q)
  | Disj (p, q) -> Conj (nnfneg p, nnfneg q)

let nnf_alt = nnfpos

(* 4.17 - Conjunctive Normal Form (CNF)
    AKA the maxterm representation of a Boolean expression 
*)

(* Computes the disjunction `p | q` in CNF
    The assumption is that the propositions are already in the Negation Normal Form (only atoms can be negated) *)
let rec distrib = function
  | p, Conj (q, r) -> Conj (distrib (p, q), distrib (p, r))
  | Conj (q, r), p -> Conj (distrib (q, p), distrib (r, p))
  | p, q -> Disj (p, q) (* no conjunctions *)

let rec cnf = function
  | Conj (p, q) -> Conj (cnf p, cnf q)
  | Disj (p, q) -> distrib (cnf p, cnf q)
  | p -> p (* a literal *)

exception NonCNF

(* Returns the list of atoms  *)
let rec positives = function
  | Atom a -> [ a ]
  | Neg (Atom _) -> []
  | Disj (p, q) -> positives p @ positives q
  | _ -> raise NonCNF

let rec negatives = function
  | Atom _ -> []
  | Neg (Atom a) -> [ a ]
  | Disj (p, q) -> negatives p @ negatives q
  | _ -> raise NonCNF

(* From 3.13 *)
let rec inter = function
  | [], _ -> []
  | x :: xs, ys ->
    if List.mem x ys then
      x :: inter (xs, ys)
    else
      inter (xs, ys)

let rec taut = function
  | Conj (p, q) -> taut p && taut q
  | p -> 0 <> List.length(inter (positives p, negatives p))

(* Exercise 4.36  A proposition in Conjunctive Normal Form can
    be represented as a list of lists of literals (atoms or their negations).
    The outer list is a conjunction; each inner list is a disjunction.
    Write functions to convert a proposition into CNF using this representation
*)

let rec flatten_disj prop : string list =
  match prop with
  | Atom a -> [ a ]
  | Neg (Atom a) -> [ "~" ^ a ]
  | Disj (p, q) -> (flatten_disj p) @ (flatten_disj q)
  | p ->
    Printf.printf "NonCNF: %s\n" (show_explicit p);
    raise NonCNF

let rec flatten_conj prop : prop list =
  match prop with
  | Conj (p, Conj (q, r)) -> flatten_conj p @ flatten_conj q @ flatten_conj r
  | Conj (Conj (q, r), p) -> flatten_conj q @ flatten_conj r @ flatten_conj p
  | Conj (p, q) -> [ p ; q ]
  | p -> [ p ]

let list_of_prop (prop : prop) : (string list) list = 
  let cnf_of_prop = cnf (nnf_alt prop) in
  let list_of_conj = flatten_conj cnf_of_prop in
  List.map
    (fun b -> flatten_disj b)
    list_of_conj