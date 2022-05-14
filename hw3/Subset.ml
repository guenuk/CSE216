(* Universal Set
*)
module type ISetUniverse = sig
    type t
    val universe: t list
end

(* SubsetPrimesMorphism: Morphism between Subset and Bool
*)
module SubsetPrimesMorphism
    (SetUniverse:ISetUniverse) : (IPrimesMorphism with type t = SetUniverse.t list) = struct

    let primes = [2; 3; 5; 7; 11; 13; 17; 19; 23]
    
    type t = SetUniverse.t list

    (*TODO: and_identity is SetUniverse.universe
    *)
    let and_identity = SetUniverse.universe

    (*TODO: find the element that satisfies pred and
            its index in list.
            Return the result in a tuple of (element, index)
            e.g. find (fun x -> x = c) [a; b; c; d] should return (c, 2)            
    *)
    let find pred list = 
        let rec help index list =
            match list with
            |[] -> assert false
            |hd::tl -> 
                if pred hd then (hd,index)
                else help (index+1) tl 
        in
        help 0 list 

    (*TODO: find the element in list at index i
            e.g. elementAt 2 [a; b; c; d] should return c
    *)
    let rec elementAt i list =
        let rec help target index l= 
            match l with 
            |[] -> assert false
            | hd::tl -> 
                if target=index then hd
                else help target (index+1) tl
        in
        help i 0 list 

    (*TODO: convert a subset to the product of corresponding primes
        e.g. {a, c, d} of {a, b, c, d} => 2*5*7
    *)
    let rec m2p lst = 
        let rec help ret lst=
            match lst with
            | [] -> ret
            | hd::tl -> 
                let (e,i) =find (fun x-> x =hd) and_identity in
                help (ret*(elementAt i primes)) tl in
        help 1 lst

        (* let rec help lst univ i ret = 
            match lst,univ with
            |[],[] -> assert false
            |hd::tl, hd2::tl2 -> 
                if hd = [] then ret
                else if hd = hd2 then help tl tl2 (i+1) (ret*(elementAt i primes))          
                else help lst tl2 (i+1) ret 
            in
        help lst [1;2;3;4;5] 0 1 *)
        (* let rec help lst primes univ ret=
            match lst, primes, univ with
            |hd::tl, hd2::tl2, hd3::tl3 -> 
                if hd = [] then ret
                else if hd = hd3 then help tl tl2 tl3 (ret*hd2)
                else help tl tl2 tl3 ret in
        help lst primes SetUniverse.universe 1 *)


    (*TODO: convert a product of primes to a corresponding subset
        e.g. 2*3*7 => {a, b, d} of {a, b, c, d} 
    *)
    let p2m n = 
        let rec help n i lst=
            if n<=1 then lst
            else if (n mod (elementAt i primes))=0 then help (n/(elementAt i primes)) (i+1) (lst@[elementAt i and_identity])
            else help n (i+1) lst 
        in
        help n 0 []
        (*     
        let rec help n univ primes subset =
            match univ, primes with
            | hd::tl, hd2::tl2 -> 
                if hd = [] then subset  
                else if n mod hd2 = 0 then help (n / hd2) tl tl2 (hd::subset)               
                else help n tl tl2 subset in
        help n and_identity primes [] *)
end

(* test cases*)
let testSubset() = 
    let _ = Printf.printf "%s\n%s\n"
        "--------------------------------"
        "Subset testing..." in
    (* TODO: Subset is BooleanAlgebra whose universe is ['a'; 'b'; 'c'; 'd']*)
    let module Subset = BooleanAlgebraBuilderByPrimes(SubsetPrimesMorphism(
        struct 
            type t =  char
            let universe = ['a';'b';'c';'d']  
        end)) in
    (* Printf.printf "DEBUG"; *)
    let module TBASubset = TestBooleanAlgebra(Subset) in
    (* Printf.printf "DEBUG"; *)
    let _ = TBASubset.testAll ['a'; 'b'] ['b'; 'c'] ['a'; 'b'; 'c'] in
    (* Printf.printf "DEBUG";   *)
    let module SubsetDual = BooleanAlgebraBuilderByDual(Subset) in
    (* Printf.printf "DEBUG"; *)
    let module TBASubset = TestBooleanAlgebra(SubsetDual) in
    (* Printf.printf "DEBUG"; *)
    let _ = TBASubset.testAll ['a'; 'b'] ['b'; 'c'] ['a'; 'b'; 'c'] in

    let _ = Printf.printf "%s\n%s\n"
        "Subset testing done: success!"
        "--------------------------------" in
    ()
