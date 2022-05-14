(* Boolean Algebra
*)
module type IBooleanAlgebra = sig 
    type t
    val or_identity: t
    val and_identity:  t
    val or_:  t -> t -> t
    val and_: t -> t -> t
    val not_: t -> t
end
