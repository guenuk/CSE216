(*-------------------------------------
  Common utility functions
-------------------------------------*) 
(* types ------------------------------
*)
type vector = float * float * float;;
type pose = float * float * float * float * float;;
type basis = vector * vector * vector * vector;;
type joint = {basis:basis; drawer:basis->unit; child:joint list};;

(* global variables -------------------
*)

(*vectors*)
let gv_o = (0.,0.,0.)
let gv_x = (1.,0.,0.)
let gv_y = (0.,1.,0.)
let gv_z = (0.,0.,1.)

(*basis*)
let gb_universe = (gv_o, gv_x, gv_y, gv_z)

(* helper functions -------------------
*)
let pi = 3.141592 
let pi2 = pi /. 2. 
let rad2deg a = a *. 180. /. pi 
let deg2rad a = a /. 180. *. pi 
let curry3 f (a, b, c) = f a b c 

let abs x = if x < 0. then -. x else x
let equ x y = abs (x -. y) < 1e-10
