(*
   stream type
*)
type 'a stream = Nil | Cons of 'a * (unit -> 'a stream);;


let car s = 
    match s with 
    | Cons (x, _) -> x
    | Nil -> assert false

let cdr s =     
    match s with 
    | Cons (_, f) -> f ()
    | Nil -> assert false

(*
   stream of constants
*)   
(* TODO: make a stream of constant c
   e.g. [c;c;c;...]*)
let rec const_sream c =  Cons (c, fun()-> const_sream c+1)

(*TODO: using const_stream, make a stream of 0s*)
let rec zeros = const_sream 0
(*TODO: using const_stream, make a stream of 1s*)
let rec ones  = const_sream 1


(* TODO: return a stream such that
    for the first delay elements return curr
    for the next elemetns return from stream
   e.g.) after_delay 3 0 [1;1;0;0;...] -> [0;0;0;1;1;0;0;...]   
*)
let rec after_delay delay curr stream =
   let rec help i =
      if i = delay then stream else Cons (curr, fun() -> help (i+1)) in
   help 0
    
(*
   not gate
   TODO: return the neation of the input stream after
   not_gate_delay. Return 0 until not_gate_delay.
   e.g.) if stream is [0;0;0;1;1;1...], then its output stream is
   [0;...;0;1;1;1;0;0;0...], where 0;...;0 means ten 0s.
*)    
let rec not_gate stream =
    let logical_not = fun a -> if a = 1 then 0 else 1 in
    let not_gate_delay = 10 in
    let rec iter strm_a = Cons (logical_not (car strm_a), fun () -> iter (cdr strm_a)) in
    after_delay not_gate_delay 0 (iter stream) 

(*
   and gate
   TODO: return the and of the input streams after
   and_gate_delay. Return 0 until and_gate_delay.
*) 
let rec and_gate stream_a stream_b = 
    let logical_and = fun a b -> if a = 1 && b = 1 then 1 else 0 in
    let and_gate_delay = 12 in
    let rec iter strm_a strm_b = Cons (logical_and (car strm_a) (car strm_b), fun () -> iter (cdr strm_a) (cdr strm_b)) in
   after_delay and_gate_delay 0 (iter stream_a stream_b)
      


(*
   or gate
   TODO: return the or of the input streams after
   or_gate_delay. Return 0 until or_gate_delay.
*) 
let rec or_gate stream_a stream_b = 
    let logical_or = fun a b -> if a = 1 || b = 1 then 1 else 0 in
    let or_gate_delay  = 13 in
    let rec iter strm_a strm_b = Cons (logical_or (car strm_a) (car strm_b),fun () -> iter (cdr strm_a) (cdr strm_b)) in
    after_delay or_gate_delay 0 (iter stream_a stream_b)


(*
   half adder
   TODO: return a tuple of sum stream and carry stream
*) 

(* let half_adder sim a b s c = 
    let d = make_wire () in
    let e = make_wire () in
     or_gate sim a b d;
    and_gate sim a b c;
    not_gate sim c e;
    and_gate sim d e s *)

let half_adder strm_a strm_b = 
   let d = or_gate strm_a strm_b in
   let c = and_gate strm_a strm_b in
   let e = not_gate c in
   let s = and_gate d e in
   (s,c)
(*
   full adder
   TODO: return a tuple of sum stream and carry stream
*) 

(* 
let full_adder sim a b cin sum cout =   
    let s  = make_wire () in
    let c1 = make_wire () in
    let c2 = make_wire () in
    half_adder sim b cin s c1;
    half_adder sim a s sum c2;
       or_gate sim c1 c2 cout *)

let full_adder strm_a strm_b strm_cin = 
   let (s,c1) = half_adder strm_b strm_cin in
   let (sum, c2) = half_adder strm_a s in
   let cout = or_gate c1 c2 in
   (sum, cout)


(*
   simulation
   print the contents of sum stream and carry stream
*) 
let simul () = 
    let (strm_sum, strm_carry) = full_adder ones ones ones in
    let maxt = 200 in
    (*param: time, sum stream, carry stream, prev sum, prev carry*)
    let rec iter t strm_s strm_c ps pc = 
        let cs = car strm_s in (*current sum*)
        let cc = car strm_c in (*current carry*)
        
        (* print sum and carry*)
        if (cs <> ps) || (cc <> pc)
        then Printf.printf "[t:%d] s:%d, c:%d\n" t cs cc
        else ();

        if t=maxt then () 
        else iter (t+1) (cdr strm_s) (cdr strm_c) cs cc in

        (* TODO: run the simulation for the rest of sum and carry
        *)


    iter 0 strm_sum strm_carry 2 2 (*2 2: to print the initial condition*) 


(* expected result
[t:0] s:0, c:0
[t:25] s:1, c:1
[t:59] s:0, c:1
[t:68] s:1, c:1
- : unit = ()
(* *)
let rec iter str i =
   if i=0 then Printf.printf "%d" i
   else begin 
      Printf.printf "%d" (car str);
      iter (cdr str) (i-1);
   end


let _ = iter (or_gate ones zeros) 28
let (s,c) = half_adder ones ones
let _ = iter s 80
let _ = iter c 80  *)
let _ = simul ()
