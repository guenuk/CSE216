(*-------------------------------------
  Drawers: board and robot
-------------------------------------*)  
(*draw board*) 
let draw_board b_camera board = 
    let dv = (0., 0., 0.05) in
    let pos i = mark_pos i in
    let draw_mark  = draw_box 0.05 0.05 0.05 in
    let draw_plate = draw_box 0.18 0.18 0.05 black in
    let rec iter i =
        let m = getmark board i in
        if m = mark_n
        then ()
        else draw_mark (mark_clr m)
                       (b_wrt_basis (v_add (pos i) dv, gv_x, gv_y, gv_z) b_camera);
        draw_plate (b_wrt_basis (pos i, gv_x, gv_y, gv_z) b_camera);
        if i < 10 then iter (i+1) else () in
    iter 0 

(*draw robot*)   
let draw_robot b_camera robot pose  = 
    let rec iter b_parent joint =
        let b = b_wrt_basis joint.basis b_parent in
        joint.drawer b;
        List.iter (fun j -> iter b j)
                  joint.child in
    iter b_camera (robot pose) 

(*draw robot and board*)
let draw b_camera robot pose board =
    clear_graph ();
    draw_robot b_camera robot pose;
    draw_board b_camera board