(*-------------------------------------
   Robot pose
-------------------------------------*)

(* #use "common.ml"
#use "draw_box.ml"
#use "basis.ml"
#use "board.ml" *)


(*the specified angle of the pose*)
let getpose (b, a1, a2, f, m) = 
        fun str -> match str with 
        | "base" -> b
        | "arm1" -> a1
        | "arm2" -> a2
        | "finger" -> f
        | "mark" -> m
        |_ -> assert false 

    (*TODO: return b, a1, a2, f, and m for the parameters
            "base", "arm1", "arm2", "finger", and "mark"
        e.g. getpose (0., 1., 2., 3., 1.) "arm2"
        should return 2. *)

(* the pose whose joint is changed by delta *)
let chgpose (b, a1, a2, f, m) joint delta = 
        match joint with
        | "base" -> (b+.delta, a1, a2, f, m)
        | "arm1" -> (b, a1+.delta, a2, f, m)
        | "arm2" -> (b, a1, a2+.delta, f, m)
        | "finger" -> (b, a1, a2, f +.delta, m)
        | "mark" -> (b, a1, a2, f, delta)
        |_ -> assert false 
    (*TODO: return the pose whose angles are updated to
            b+delta, a1+delta, a2+delta, f+delta, or delta
            depending on the joint parameters of
            "base", "arm1", "arm2", "finger", and "mark"
        e.g.  chgpose (0., 1., 2., 3., 1.) "arm2" 1.
        should return (0., 1., 3., 3., 1.)*)

(* find the angle joints to get to x y z*)


let find_pose x y z = 
    fun f m ->
        let d1 = 0.5 in  
        (*length of arm1*)
        let d2 = 0.55 in 
        (* length of arm2 + length of finger *)
         
        let d = sqrt(x*.x +. y*.y +. z*.z) in

        let delta = asin(z /. d) in
        let alpha = acos((d1 ** 2. +. d2 ** 2. -. d ** 2. ) /. (-2. *. d1 *. d2)) in
        let theta = asin((d2 *. sin(alpha)) /. d) in

        let a1 = rad2deg (pi2 -. theta -. delta) in
        let a2 = rad2deg (alpha) in
        let b = rad2deg (atan2 y x) in

        (b, a1, a2, f, m)

        (*TODO: find b, a1, and a2 and return the pose (b, a1, a2, f, m)
                b:  angle (deg) of base measured from x axis (use atan2),
                a1: angle (deg) of arm1 measured from z axis
                a2: angle (deg) of arm2 measured from arm1
            e.g. find_pose 0.1 0.1 0.1 1. 2. should return 
            (45.000, -27.800, 161.805, 1., 2.)
            Please see the lecture slide for more information *)



(*the pose between movements*)
let lift_pose pose = 
    let b = getpose pose "base"   in
    let f = getpose pose "finger" in
    let m = getpose pose "mark"   in
    (b, 10., 70., f, m)

(*the robot at x y z position*)
let newrobot x y z = 
    fun pose -> 
        let mark = {
                basis  = ((0.,0.,0.55), gv_x, gv_y, gv_z);
                drawer = draw_box 0.05 0.05 0.05 (mark_clr (getpose pose "mark"));
                child  = []} in
        let finger1 = {
                basis  = b_rotx (getpose pose "finger") 
                                ((0., -0.03,0.5), gv_x, gv_y, gv_z);
                drawer = draw_box 0.02 0.02 0.1 black;
                child  = []} in
        let finger2 = {
                basis  = b_rotx (-. getpose pose "finger")
                                ((0., 0.03,0.5), gv_x, gv_y, gv_z);
                drawer = draw_box 0.02 0.02 0.1 black;
                child  = []} in
        let arm2 = {
                basis  = b_roty (getpose pose "arm2")
                                ((0.,0.,0.5), gv_x, gv_y, gv_z);
                drawer = draw_box 0.10 0.10 0.5 black;
                child  = if getpose pose "mark" = mark_n
                         then [finger1; finger2;]
                         else [finger1; finger2; mark]} in
        let arm1 = {
                basis  = b_roty (getpose pose "arm1")
                                ((0.,0.,0.1), gv_x, gv_y, gv_z);
                drawer = draw_box 0.12 0.12 0.5 black;
                child  = [arm2]} in    
        let base = {
                basis  = b_rotz (getpose pose "base")
                                ((x, y, z), gv_x, gv_y, gv_z);
                drawer = draw_box 0.3 0.3 0.1 black;
                child  = [arm1]} in
        base

(*unit test*)
let test_robot_pose () =
    let pose = (90., 30., 60., 0., mark_n) in 
    let test pose (b, a1, a2, f, m) =
        assert (b  = getpose pose "base");  
        assert (a1 = getpose pose "arm1");  
        assert (a2 = getpose pose "arm2");  
        assert (f  = getpose pose "finger");  
        assert (m  = getpose pose "mark") in

    Printf.printf("----------------------------------------\n");
    Printf.printf("test robot pose...\n");
    test pose (90., 30., 60., 0., mark_n);      

    test (chgpose pose "base" 5.)     (95., 30., 60., 0., mark_n);  
    test (chgpose pose "arm1" 5.)     (90., 35., 60., 0., mark_n);  
    test (chgpose pose "arm2" 5.)     (90., 30., 65., 0., mark_n);  
    test (chgpose pose "finger" 5.)   (90., 30., 60., 5., mark_n);  
    test (chgpose pose "mark" mark_o) (90., 30., 60., 0., mark_o);   
    

    let p = (curry3 find_pose (mark_pos 0)) 3. mark_o in
    let a = (75.963772335887612, 49.548498880311648, 76.5958765584046, 3., mark_o) in
    test p a;
    Printf.printf("test robot pose\n")
let _ = test_robot_pose ()
