(* NOTE: it is not possible to include a functor type here (i.e., `include Types.MakeParser`
    complains that Types.MakeLexer is not a module type, even though it seems to be) *)
module Make : Types.MakeLexer