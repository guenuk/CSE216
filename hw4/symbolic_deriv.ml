type var = X | Y | Z    (*variable type*)

type expr = N of int    (*expression type*)
          | V of var
          | Add of expr * expr
          | Mul of expr * expr

(*add two exprs*)
let add a b = 
    match (a, b) with
    | (N 0, y) -> y
    | (x,   N 0) -> x
    | (N x, N y) -> N (x + y)
    | _ -> Add (a, b)

(*multiply two exprs*)
let mul a b = 
    match (a, b) with
    | (N 0, y) -> N 0
    | (N 1, y) -> y
    | (x,   N 0) -> N 0
    | (x,   N 1) -> x
    | (N x, N y) -> N (x * y)
    | _ -> Mul (a, b)


(*symbolic derivative of expr w.r.t. var
*)
   
(* let rec deriv expr var =
    match expr with
    | N x -> N 0
    | V v -> if v = var then N 1 else N 0
    | Add (a, b) -> add (deriv a var)(deriv b var)
    | Mul (a, b) -> add (mul a (deriv b var))(mul (deriv a var) b) *)


(*the identity continuation
*)
let id x = x 

(*TODO: rewrite deriv in CPS
*)


let rec deriv expr var =
    let rec derivH expr var k =
        match expr with
        | N x -> k (N 0)
        | V v -> if v = var then k (N 1) else k (N 0)
        | Add (a,b) -> derivH a var (fun x-> derivH b var (fun y-> k (add x y)))
        | Mul (a,b) -> derivH b var (fun x-> derivH a var (fun y -> k (add (mul a x) (mul y b)))) in
    derivH expr var (id)



(*     

let rec to_str expr =
    let open Printf in
    let var_to_str = function
        X -> "x" | Y -> "y" | Z -> "z" in
    match expr with
    | N a -> sprintf "%d" a
    | V v -> var_to_str v
    | Add (a, b) -> sprintf "(%s + %s)" (to_str a) (to_str b)
    | Mul (a, b) -> sprintf "%s * %s"   (to_str a) (to_str b) *)

(*TODO: rewrite to_str in CPS
*)

let rec to_str expr = 
    let open Printf in
    let var_to_str = function
        X -> "x" | Y -> "y" | Z -> "z" in
    let rec to_strH expr k = 
        match expr with
        |N a -> k (sprintf "%d" a)
        |V v -> k (var_to_str v)
        |Add (a,b) -> to_strH a (fun x-> to_strH b (fun y -> k( sprintf "(%s + %s)" x y)))
        |Mul (a,b) -> to_strH a (fun x-> to_strH b (fun y -> k( sprintf "%s * %s" x y)))in
    to_strH expr id

(* 됐다!!!!!!!!!!!!!!!!!!! *)




(*test cases*)    
let a = add (V X) (N 3)
let _ = to_str a                 (* (x + 3) *)
let _ = to_str (deriv a X)       (* 1 *)
let _ = to_str (deriv a Y)       (* 0 *)

let b = add (mul (N 2) (V X))
            (mul (V Y) (N 3))
let _ = to_str b                  (* (2 * x + y * 3) *)
let _ = to_str (deriv b X)        (* 2 *)
let _ = to_str (deriv b Y)        (* 3 *)

let c = mul (mul (V X) (V Y))
            (add (V X) (N 3))
let _ = to_str c                  (* x * y * (x + 3) *)
let _ = to_str (deriv c X)        (* (x * y + y * (x + 3)) *)
let _ = to_str (deriv c Y)        (* x * (x + 3) *)
