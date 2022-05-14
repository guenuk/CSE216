(*-------------------------------------
  Vector 3D
-------------------------------------*)
(* #use "common.ml" *)

(*vector addition*)
let v_add (x, y, z) (u, v, w) = (x +. u , y +. v, z +. w);;

(*vector subtraction*)
let v_sub (x, y, z) (u, v, w) = (x -. u, y -.v, z-.w);;

(*scalar multiplication*)
let v_smul s (x, y, z) = (s *. x, s *. y, s *. z);;

(*inner product*)
let v_prod (x, y, z) (u, v, w) = x *. u +. y *. v +. z *.w;; 

(*length of a vector*)
let v_len v = sqrt (v_prod v v);;
(*TODO: implement this function using v_prod*)

(*vector rotation*)
let v_rot2d ang (x, y) = 
    let r = deg2rad ang in (*a in deg to r in radian*)
    ( (cos r)*.x -. (sin r)*.y,
      (sin r)*.x +. (cos r)*.y ) 

let v_rotx ang (x, y, z) =
    let (u, v) = v_rot2d ang (y, z) in
    (x, u, v)

let v_roty ang (x, y, z) =
    let (u, v) = v_rot2d ang (z, x) in
    (v, y, u)

let v_rotz ang (x, y, z) =
    let (u, v) = v_rot2d ang (x, y) in
    (u, v, z)

(*unit test*)
let test_vector () =
    Printf.printf("----------------------------------------\n");
    Printf.printf("test vector...\n");
    assert ((3.,5.,7.) = v_add (2.,3.,4.) (1.,2.,3.));  
    assert ((1.,1.,1.) = v_sub (2.,3.,4.) (1.,2.,3.));  
    assert ((2.,4.,6.) = v_smul 2. (1.,2.,3.));  
    assert (20. = v_prod (2.,3.,4.) (1.,2.,3.));  
    assert (equ 5. (v_len (0.,3.,4.)));
    Printf.printf("test vector done\n")
let _ = test_vector ()
