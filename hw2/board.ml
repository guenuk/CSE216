(*-------------------------------------
  Board and Mark
-------------------------------------*)
(* 
#use "basis.ml"

#load "graphics.cma"
#load "unix.cma"
#directory "+threads"
#load "threads.cma"

open Graphics *)
(* mark -------------------------------
*)   
(*the marks are floats to match with other pose elements*)
let mark_n = 0. 
let mark_o = 1. 
let mark_x = 2.         

(*mark to color map*)
let mark_clr m = 
    if      m = mark_n then black
    else if m = mark_o then red
    else if m = mark_x then blue
    else assert false 
    
(* board ------------------------------
*)   
(*the position of the mark at index i*)   
let mark_pos i =
    match i with
    | 0 -> ( 0.2, 0.8, 0.)
    | 1 -> (  0., 0.8, 0.)
    | 2 -> (-0.2, 0.8, 0.)
    | 3 -> ( 0.2, 0.6, 0.)
    | 4 -> (  0., 0.6, 0.)
    | 5 -> (-0.2, 0.6, 0.)
    | 6 -> ( 0.2, 0.4, 0.)
    | 7 -> (  0., 0.4, 0.)
    | 8 -> (-0.2, 0.4, 0.)
    | 9 -> ( 0.5, 0.4, 0.)
    | 10-> ( 0.5, 0.6, 0.)
    | _ -> assert false 

(*get the i-th mark of the board*)
let getmark board i =
    let rec helper board i = 
        match board with
        | [] -> assert false
        | hd :: tl -> 
                    if i = 0 then hd
                    else helper tl (i-1) in
    helper board i;;
    (*TODO: implement this function
      getmark [mark_o; mark_n; mark_x; mark_o; ...] 2 should be mark_x*)

(*the board whose i-th mark is switched to m*)
let chgmark board i m =
    let rec helper board i m =
        match board with 
        | [] -> board
        | hd :: tl ->
                    if i=0 then m::(helper tl (i-1) m)
                    else hd::(helper tl (i-1) m) in
    helper board i m;;
    (*TODO: implement this function
      chgmark [mark_o; mark_n; mark_x; mark_o; ...] 2 mark_n
      should be [mark_o; mark_n; mark_n; mark_o; ...]*)

(*print the board for the debugging*)
let print_board board = 
    let str m = if      m = mark_o then "o"
                else if m = mark_x then "x"
                else                    " " in
    let pm = fun i -> str (getmark board i) in
    Printf.printf "%s, %s, %s\n"   (pm 0) (pm 1) (pm 2);
    Printf.printf "%s, %s, %s\n"   (pm 3) (pm 4) (pm 5);
    Printf.printf "%s, %s, %s\n\n" (pm 6) (pm 7) (pm 8)

(*unit test*)
let test_board () =
    let board = [ mark_n; mark_n; mark_n;
                  mark_n; mark_n; mark_n;
                  mark_n; mark_n; mark_n;
                  mark_o (*9*); mark_x (*10*)] in
    Printf.printf("----------------------------------------\n");
    Printf.printf("test board...\n");
    assert(mark_x = getmark board 10);
    assert(mark_n = getmark board 5);
    assert(mark_o = (board |> fun b -> chgmark b 5 mark_o
                           |> fun b -> getmark b 5));
    Printf.printf("test board done\n")
let _ = test_board ()
