module type QUEUE = sig
  type 'a t

  exception E

  val empty : 'a t

  val null : 'a t -> bool

  val hd : 'a t -> 'a

  val enq : 'a t * 'a -> 'a t

  val deq : 'a t -> 'a t
end