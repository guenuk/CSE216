(*-------------------------------------
  App
-------------------------------------*)

(*https://caml.inria.fr/pub/docs/manual-caml-light/node16.html*)
(*https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora050.html*)

(* #use "topfind" *)
#load "graphics.cma"
#load "unix.cma"
#directory "+threads"
#load "threads.cma"

open Graphics

#use "common.ml"
#use "vector.ml"
#use "basis.ml"
#use "draw_box.ml"
#use "board.ml"
#use "robot_pose.ml"
#use "draw_config.ml"
#use "robot_command.ml"
#use "game.ml"

let app () =
    (*camera basis*)
    let b_camera = (b_rotx (-60.) (b_rotz (-210.) gb_universe)) in

    (*initial pose*)
    let ipose = (90., 30., 60., 0., mark_n) in

    (*initial board*)
    let iboard = [ mark_n; mark_n; mark_n;
                mark_n; mark_n; mark_n;
                mark_n; mark_n; mark_n;
                mark_o (*9*); mark_x (*10*)] in

    open_graph " 800x800";
    print_result (game b_camera (newrobot 0. 0. 0.) (ipose, iboard))

let _ = app ()

