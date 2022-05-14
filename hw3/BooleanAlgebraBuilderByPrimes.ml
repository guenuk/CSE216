(*isomorphism between Primes and a module M
  Morphism to and from Primes
*)

module type IPrimesMorphism = sig 
    type t  (*M.t*)
    val and_identity: t (*M.and_identity*)
    val m2p: t -> int   (*M to Primes*)
    val p2m: int -> t   (*Primes to M*)
end

(* Build a Boolean Algebra M using a morphism between Primes and M
   Parameter is a morphism between Primes and M
*)
module BooleanAlgebraBuilderByPrimes
    (PMorphism: IPrimesMorphism) : (IBooleanAlgebra with type t = PMorphism.t) = struct
    module P = PrimesBuilder(struct
        let product = PMorphism.m2p PMorphism.and_identity
    end)

    (*TODO: get t from PMorphism*)
    type t = PMorphism.t 

    (*TODO: get and_identity from P.and_identity*)
    let and_identity  =  PMorphism.and_identity
    
    (*TODO: get or_identity from P.or_identity*)
    let or_identity = (PMorphism.p2m P.or_identity)
    
    (*TODO: send a, b to P; perform or_ in P; bring the result back to M*)
    let or_  a b = PMorphism.p2m (P.or_ (PMorphism.m2p a)  (PMorphism.m2p b))

    (*TODO: send a, b to P; perform and_ in P; bring the result back to M*)
    let and_ a b = PMorphism.p2m (P.and_ (PMorphism.m2p a) (PMorphism.m2p b)) 

    (*TODO: send a to P; perform not_ in P; bring the result back to M*)
    let not_ a   = PMorphism.p2m (P.not_ (PMorphism.m2p a))

end
