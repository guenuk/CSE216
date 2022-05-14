#use "IBooleanAlgebra.ml"
#use "BooleanAlgebraBuilderByDual.ml"
#use "TestBooleanAlgebra.ml"
#use "Primes.ml"
#use "BooleanAlgebraBuilderByPrimes.ml"
#use "Bool.ml"
#use "Subset.ml"

(*run tests*)
let _ = testPrimes ()
let _ = testBool ()
let _ = testSubset ()
