(* Geunuk Na/ 111000447 /geunuk.na@stonybrook.edu  *)
(* Newton's method
 *)

let newton f x0 =
    let eps = 1e-10 in

    (*absolute value of x*)
    let abs x = if x < 0. then -. x else x in

    (*find the derivative of f: (f(x + eps) - f(x)) / eps*)
    let derivative f = fun x -> ((f (x +. eps) -. f x) /. eps) in 

    (*f'(x)*) 
    let dfdx = derivative f in

    (*find the next guess from the current guess*)
    let next f = fun x -> x -. f x /. dfdx x in 


    (*fixed point of f is x such that x = f(x) *)
    let rec fixed_point f x =
	    let x1 = f x in
        if abs (x1-.x) < eps 
        then x1 
        else fixed_point f x1 in
    fixed_point (next f) x0;; 

(*sqrt of x using newton*)    
let sqrt x = newton (fun y-> y *. y -. x) x ;;

let _ = sqrt 2.