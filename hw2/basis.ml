(*-------------------------------------
  Basis
-------------------------------------*)
(* #use "vector.ml" *)
(*basis rotation*)
let b_rot ang vrot (o, x, y, z) =
    (o, vrot ang x, vrot ang y, vrot ang z) 

let b_rotx ang basis = b_rot ang v_rotx basis 

let b_roty ang basis = b_rot ang v_roty basis 

let b_rotz ang basis = b_rot ang v_rotz basis 

(*vector with respect to basis*)
let v_wrt_basis (u, v, w) (o, x, y, z) =
    o |> v_add (v_smul u x)
      |> v_add (v_smul v y)
      |> v_add (v_smul w z)

(*basis with respect to basis*)
let b_wrt_basis (o, x, y, z) basis = 
    let (bo, bx, by, bz) = basis in
    let basis' = (gv_o, bx, by, bz) in (*for axes, origin is gv_o *)
    ( (v_wrt_basis o basis),
      (v_wrt_basis x basis'),
      (v_wrt_basis y basis'),
      (v_wrt_basis z basis') ) 

