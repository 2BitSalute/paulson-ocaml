let rec minl = function
  | [] -> None
  | [ only ] -> Some only
  | fst :: snd :: rest -> if fst < snd then minl (fst :: rest) else minl (snd :: rest)

let rec maxl = function
  | [] -> None
  | [ only ] -> Some only
  | fst :: snd :: rest -> if fst > snd then maxl (fst :: rest) else maxl (snd :: rest)

let rec take ~count : 'a list -> 'a list = function
  | [] -> []
  | head :: rest -> if count > 0 then head :: (take ~count:(count - 1) rest) else []

let rec drop ~count = function
  | [] -> []
  | head :: rest when count >= 0 -> if count = 0 then head :: rest else drop ~count:(count - 1) rest
  | _ -> []

(* Paulson implements `nth` in terms of drop *)
let rec nth ~index = function
  | [] -> None
  | head :: rest when index >= 0 -> if index = 0 then Some head else nth ~index:(index - 1) rest
  | _ -> None

let rec mem element = function
  | [] -> false
  | head :: rest -> if head = element then true else mem element rest

let newmem element l = if mem element l then l else element :: l

let rec lookup ~key = function
  | [] -> None
  | (k, v) :: rest -> if k = key then Some v else lookup ~key rest

let rec filter predicate = function
  | [] -> []
  | head :: rest -> if predicate head then head :: filter predicate rest else filter predicate rest

let rec exists predicate = function
  | [] -> false
  | head :: rest -> if predicate head then true else exists predicate rest

let rec forall predicate = function
  | [] -> true
  | head :: rest -> if predicate head then forall predicate rest else false

let rec foldleft f acc = function
  | [] -> acc
  | head :: rest -> foldleft f (f acc head) rest

let rec foldright f acc = function
  | [] -> acc
  | head :: rest -> f head (foldright f acc rest)