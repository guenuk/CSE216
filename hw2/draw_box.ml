(*-------------------------------------
  Drawer: box
-------------------------------------*)
(* #use "basis.ml"

#load "graphics.cma"
#load "unix.cma"
#directory "+threads"
#load "threads.cma"

open Graphics *)

let draw_box w d h clr = 
    let scr_coord (x, y, z) =
        let s = 600. in
        let p = (2. +. z) /. 3. in (*perspective*)
        (truncate (s *. x *. p) + 500,
         truncate (s *. y *. p) + 400) in 
    let p0 = (-.w /.2., -.d /.2., 0.) in
    let p1 = (  w /.2., -.d /.2., 0.) in
    let p2 = (  w /.2.,   d /.2., 0.) in
    let p3 = (-.w /.2.,   d /.2., 0.) in
    let p4 = (-.w /.2., -.d /.2., h ) in
    let p5 = (  w /.2., -.d /.2., h ) in
    let p6 = (  w /.2.,   d /.2., h ) in
    let p7 = (-.w /.2.,   d /.2., h ) in

    fun basis ->
        let (x0, y0) = scr_coord (v_wrt_basis p0 basis) in
        let (x1, y1) = scr_coord (v_wrt_basis p1 basis) in
        let (x2, y2) = scr_coord (v_wrt_basis p2 basis) in
        let (x3, y3) = scr_coord (v_wrt_basis p3 basis) in
        let (x4, y4) = scr_coord (v_wrt_basis p4 basis) in
        let (x5, y5) = scr_coord (v_wrt_basis p5 basis) in
        let (x6, y6) = scr_coord (v_wrt_basis p6 basis) in
        let (x7, y7) = scr_coord (v_wrt_basis p7 basis) in
        set_color clr;
        moveto x3 y3; lineto x0 y0; lineto x1 y1; lineto x2 y2; lineto x3 y3;
        moveto x7 y7; lineto x4 y4; lineto x5 y5; lineto x6 y6; lineto x7 y7;
        moveto x0 y0; lineto x4 y4;
        moveto x1 y1; lineto x5 y5;
        moveto x2 y2; lineto x6 y6;
        moveto x3 y3; lineto x7 y7 
        