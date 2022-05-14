(*-------------------------------------
  Robot commands
-------------------------------------*)
(* 
#use "draw_config.ml"
(*  *)
#use "robot_pose.ml"
#use "board.ml" *)
(*move from pose to target_pose*)
let moveto_pose b_camera robot (pose, board) target_pose =    
    let db  = (getpose target_pose "base")   -. (getpose pose "base") in
    let da1 = (getpose target_pose "arm1")   -. (getpose pose "arm1") in
    let da2 = (getpose target_pose "arm2")   -. (getpose pose "arm2") in
    let df  = (getpose target_pose "finger") -. (getpose pose "finger") in
    
    (*move the joint <ang> angle in <step> steps
      e.g. rotate arm1 30 deg in 5 steps
           => rotate arm1 5 times 6 deg each
    *)
    let rot_joint pose joint ang step =

        let rec helper pose1 count =
            let chP = chgpose pose1 joint (ang /. (float)(step)) in
            draw b_camera robot pose1 board;
            Thread.delay 0.05 ;
            if count = 0 then pose1
            else helper chP (count-1) in
        
        helper pose step in

        (*TODO: implement this method
            - on each step, draw the robot and the board
            - wait for 50ms by calling Thread.delay 0.05
            - then either return pose or rotate more
        *)

    (*move the joints in base, arm1, arm2, and finger order*)
    let p = pose |> fun p -> rot_joint p "base" db  5
                 |> fun p -> rot_joint p "arm1" da1 5
                 |> fun p -> rot_joint p "arm2" da2 5
                 |> fun p -> rot_joint p "finger" df 3 in
    (p, board)

let pick (pose, board) i =
    let f = getpose pose "finger" in
    let m = getmark board i in
    (* let chgP = if f = 0. then 0. else -10. in *)
    let p = chgpose pose "finger" (-. f) |> fun p -> chgpose p "mark" m in  
    (* let rec helper board i =
        match board with
        | [] -> []
        | hd::tl -> if i=0 then helper tl (i-1)
                    else hd::helper tl (i-1) in *)
    let b = chgmark board i mark_n in
    
    (p,b)
    (*TODO: return (p, b), where p is the pose whose
            mark and finger are switched to m and 0. respectively
            and b is the board whose mark at i is removed.
        e.g.     pick ((1.,1.,1.,10.,mark_n), [mark_x;mark_o;...]) 1
        should return ((1.,1.,1.,0., mark_o), [mark_x;mark_n;...])
    *)

(*dropt the mart at index i*)
let drop (pose, board) i =
    let f = getpose pose "finger" in
    let m = getpose pose "mark" in
    let j = if m = mark_o then 9 else 10 in
(* 
    let chgP = if f = 0. then 10. else -10. in *)
    let b = chgmark (chgmark board i m) j m in
    
    let p = (chgpose pose "finger" 10.) |> fun po -> chgpose po "mark" mark_n in

    (* let p = chgpose (chgpose pose "mark" mark_n) "finger" chgP in *)

    (p,b)

    (*TODO: return (p, b),
            where b is board with its i-th and j-th marks replaced by m
            and p is pose whose mark and finger are switched
            to mark_n and 10. respectively
        e.g.     drop ((1.,1.,1.,0., mark_o), [mark_x;mark_n;...]) 1
        should return ((1.,1.,1.,10.,mark_n), [mark_x;mark_o;...])
    *)



let mark b_camera robot (pose, board) mrk i =
    let f = getpose pose "finger" in
    let m = getpose pose "mark" in
    let j = if mrk = mark_o then 9 else 10 in
    (*moveto_pose with the first two params passed*)
    let mvp = moveto_pose b_camera robot in

    let ipose = curry3 find_pose (mark_pos i) f m in
    let jpose = curry3 find_pose (mark_pos j) f m in

    (*TODO: 1) find b, a1, and a2 for ipose and jpose
        using curry3, find_pose, mark_pos then
        2) pass two params for the fun returned by find_pose
     *)        
    (* let ipose = ip f m in
    let jpose = jp f m in *)
    let getP (p,b) = p in 
    let getB (p,b) = b in  

    (* let s1 = mvp (pose, board) jpose in 
    
    let s2 = pick s1 j in 
    
    let s3 = lift_pose (getP s2) in
    
    let s4 = mvp (s3,(getB s2)) ipose in
    
    let s5 = drop s4 i in
    
    let s6 = lift_pose (getP s5) in *)

    (* mvp (pose, board) jpose;
    pick (pose, board) j;
    lift_pose pose;
    mvp (pose, board) ipose;
    drop (pose, board) i;
    lift_pose pose;
    (pose, board) *)
    
    (* let (p1, b1) = mvp (pose, board) jpose in
    let (p2, b2) = pick (p1, b1) j in
    let p3 = lift_pose p2 in
    let (p4, b4) = mvp (p3, b2) ipose in
    let (p5,b5) = drop (p4, b4) i in
    let p6 = lift_pose p5 in
    (p6,b5) *)

    (* mvp (pose, board) jpose;
    pick mvp (pose, board) jpose j;
    lift_pose (getP pick mvp (pose, board) jpose j);
    mvp (lift_pose (getP pick mvp (pose, board) jpose j), getB pick mvp (pose, board) jpose j) ipose;
    drop mvp (lift_pose (getP pick mvp (pose, board) jpose j), getB pick mvp (pose, board) jpose j) ipose i;
    lift_pose (getP drop mvp (lift_pose (getP pick mvp (pose, board) jpose j), getB pick mvp (pose, board) jpose j) ipose i);
    (lift_pose (getP drop mvp (lift_pose (getP pick mvp (pose, board) jpose j), getB pick mvp (pose, board) jpose j) ipose i), getB drop mvp (lift_pose (getP pick mvp (pose, board) jpose j), getB pick mvp (pose, board) jpose j) ipose i)
     *)
     
    mvp (pose,board) jpose
                 |> fun (p,b) -> pick (p,b) j 
                 |> fun (p,b) -> mvp (p,b) (lift_pose p)
                 |> fun (p,b) -> mvp (p,b) ipose
                 |> fun (p,b) -> drop (p,b) i
                 |> fun (p,b) -> mvp (p,b) (lift_pose p)
    

    (*TODO: 1. move to position jpose
            2. pick the mark at j
            3. lift
            4. move to position ipose
            5. drop the mark at i
            6. lift
            7. return the resulting pose and the board*)