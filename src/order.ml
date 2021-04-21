module type ORDER = sig
  type t
  val less : t -> t -> bool
end

(* The `with type` is necessary to be able to easily create and pass values of type `string`
    when StringOrder is used as an argument to the table functor. Otherwise,
    when you're passing in string keys to, e.g., `update`, the compiler complains that
    the expression is a `string` but a `key` expression was expected.
    
    In other words, ORDER makes `type t` abstract, but we have to make it concrete again
    so users can use this module. *)
module StringOrder : (ORDER with type t = string) = struct
  type t = string
  let less x y = (String.compare x y) < 0
end

module IntOrder : (ORDER with type t = int) = struct
  type t = int
  let less x y = x < y
end

module RealOrder : (ORDER with type t = float) = struct
  type t = float
  let less x y = x < y
end

module IntPairOrder : ORDER = struct
  type t = int * int
  let less (x1, x2) (y1, y2) = x1 < y1 || (x1 = x2 && x2 < y2)
end

module type TABLE = sig
  type key
  type 'value t

  exception Lookup

  val empty : 'value t
  val lookup : 'value t -> key -> 'value
  val update : key -> 'value -> 'value t -> 'value t
end

module TableFUN (Order : ORDER) (Tree : Tree_sig.TREE) : TABLE with type key = Order.t = struct
  type key = Order.t
  type 'value t = (key * 'value) Tree.t

  (* Aliasing the type because below I open the Table module, which also has a type t that causes
      hard-to-understand compiler errors when type annotations are used... SAD! *)
  type 'value table = 'value t

  exception Lookup

  open Tree

  let empty = Lf

  let rec lookup (t : 'value table) key : 'value =
    match t with
    | Br ((another_key, x), t1, t2) ->
      if Order.less key another_key then lookup t1 key
      else if Order.less another_key key then lookup t2 key
      else x
    | Lf -> raise Lookup

  let rec update key value (table : 'value table) : 'value table =
    match table with
    | Lf -> Br ((key, value), Lf, Lf)
    | Br ((another_key, x), t1, t2) ->
      if Order.less key another_key then Br ((another_key, x), update key value t1, t2)
      else if Order.less another_key key then Br ((another_key, x), t1, update key value t2)
      else Br ((another_key, value), t1, t2)
end

module IntTable = TableFUN (IntOrder) (Tree) 
module StringTable = TableFUN (StringOrder) (Tree)