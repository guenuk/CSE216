(* Build a Boolean Algebra M as a Dual of B
   if M is a dual of B then
    M.and_identity = B.or_identity
    M.or_identity  = B.and_identity
    M.and_         = B.or_
    M.or_          = B.and_
    M.not_         = B.not_
*)
module BooleanAlgebraBuilderByDual
    (B: IBooleanAlgebra): (IBooleanAlgebra with type t = B.t) = struct

    (*TODO: get M.type from B*)
    type t = B.t

    (*TODO: get M.and_identity from B*)
    let and_identity = B.or_identity
    
    (*TODO: get M.or_identity from B*)
    let or_identity = B.and_identity
    
    (*TODO: get M.or_ from B*)
    let or_ = B.and_

    (*TODO: get M.and_ from B*)
    let and_ = B.or_

    (*TODO: get M.not_ from B*)
    let not_ = B.not_
end
