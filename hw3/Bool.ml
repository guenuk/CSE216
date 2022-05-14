(* BoolPrimesMorphism: Morphism between Primes and Bool
*)
module BoolPrimesMorphism: (IPrimesMorphism with type t = bool) = struct
    type t = bool
    
    (*and_identity = true*)
    let and_identity = true 

    let m2p b = if b then 2 else 1 

    let p2m p = (p = 2)            
end

(* test cases*)
let testBool() = 
    let _ = Printf.printf "%s\n%s\n"
        "--------------------------------"
        "Bool testing..." in

    let module Bool = BooleanAlgebraBuilderByPrimes (BoolPrimesMorphism) in
    let module TBABool = TestBooleanAlgebra(Bool) in
    let _ = TBABool.testAll true false false in
    let _ = TBABool.testAll true false true in

    let module BoolDual = BooleanAlgebraBuilderByDual(Bool) in
    let module TBABool = TestBooleanAlgebra(BoolDual) in
    let _ = TBABool.testAll true false false in
    let _ = TBABool.testAll true false true in

    let _ = Printf.printf "%s\n%s\n"
        "Bool testing done: success!"
        "--------------------------------" in
    ()
