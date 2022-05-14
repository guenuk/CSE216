(*-------------------------------------
  Game
-------------------------------------*)

#use "board.ml";;
#use "robot_command.ml";;


(*winner of the board if any; otherwise mark_n*)
let winner board = 
    (*whether a = b = c*)
    let equ3 a b c = (a = b && b = c && a <> mark_n) in
    (*getmark with board param provided*)
    let gm = getmark board in
    (*cache for the 9 positions*)
    let (b00, b01, b02) = (gm 0, gm 1, gm 2) in
    let (b10, b11, b12) = (gm 3, gm 4, gm 5) in
    let (b20, b21, b22) = (gm 6, gm 7, gm 8) in
    
    (*return the winner if there is one; otherwise mark_n*)
    if      equ3 b00 b01 b02 then b00
    else if equ3 b10 b11 b12 then b10
    else if equ3 b20 b21 b22 then b20
    
    else if equ3 b00 b10 b20 then b00  
    else if equ3 b01 b11 b21 then b01  
    else if equ3 b02 b12 b22 then b02 
    
    else if equ3 b00 b11 b22 then b00  
    else if equ3 b02 b11 b20 then b02
    else mark_n
    
(*next mark index of m*)
let next_mark board m = 
    print_board board;
    let rec iter m c i = (*c: (candidate) 9 or an unoccupied board index*)
        let rec lastIndex i ret = 
            if i = 9 then ret
            else if getmark board i = mark_n then lastIndex (i+1) i
            else lastIndex (i+1) ret in
        let rec isFull i = 
            if i = c then true
            else if getmark board i = mark_n then false
            else isFull (i+1) in
        if isFull 0 then (c, false)
        else begin 
            let gm = getmark board i in
            let li = lastIndex 0 0 in
            if i = (li+1) then (li, false)
            else if (mark_n = gm) && winner (chgmark board i m) = m then (i, true)
            else iter m c (i+1) 
            end in


        (* if i = c then (c,false) 
        else if winner (chgmark board i m) = m then (i, true)
        else iter m c (i+1) in *)
        (*TODO: return (i, true)  if putting m at i makes m the winner
                return (c, false) if no winning position is found*)
            
    (*the other mark*)
    let n = if m = mark_o then mark_x else mark_o in
    (*get my winning chance*)
    let (mi, mw) = iter m 9 0 in 
    (*get other's winning chance*)
    let (ni, nw) = iter n 9 0 in 

    if mw      then mi (*if I can win, take the chance*)
    else if nw then ni (*if the other can win, block it*)
    else            mi (*take any empty spot*)

(*read a keyboard input*)
let rec getkey () =    
    try
        let s = Graphics.wait_next_event [Graphics.Key_pressed] in
        if s.Graphics.keypressed then
            let k = s.Graphics.key in
            if ('1' <= k && k <= '9') || (k = 'q')
            then k
            else getkey ()
        else getkey ()
    with
        _ -> 'q'

(*play the tic-tac-toe game*)
let rec game b_camera robot (pose, board) =
    (*mark at index i and let the other mark*)
    let play i = 
        (*mark O at position i*)
        let (p, b) = mark b_camera robot (pose, board) mark_o i in
        
        (*check if this change makes me the winner*)
        let w = winner b in
        if w = mark_n then (*no winner yet*)
            (*get the computer's next move*)
            let j = next_mark b mark_x in
            if j = 9 then (*no empty place: game over*)
                mark_n 
            else (*mark X at index j*)
                let (p', b') = mark b_camera robot (p, b) mark_x j in
                game b_camera robot (p', b')
        else (*w is the winner*)    
            w in
            
    (*draw the board and the robot*)
    draw b_camera robot pose board;
    
    (*check if there is any winner*)
    let w = winner board in
    if w = mark_n then (*no winner yet*)
        let k = getkey () in
        if '1' <= k && k <= '9' then
            let i = (Char.code k) - (Char.code '0') - 1 in
            (*mark at position i if it is unoccupied*)
            if getmark board i = mark_n then 
                play i
            else (*position i is already taken*)
                game b_camera robot (pose, board)
        else if k = 'q' then
            mark_n
        else
            assert false
    else (*w is the winner*)
        w 

    
(*print the game result*)
let print_result m =
    if      m = mark_o         then Printf.printf "You win! :)\n"
    else if m = mark_x then Printf.printf "You lost :(\n"
    else                    Printf.printf "No winner\n" 

(*unit test*)
let test_game () =
    let board = [ mark_n; mark_n; mark_n;
                  mark_n; mark_n; mark_n;
                  mark_n; mark_n; mark_n;
                  mark_o (*9*); mark_x (*10*)] in
    Printf.printf("----------------------------------------\n");
    Printf.printf("test game...\n");
    board |> fun b -> chgmark b 0 mark_o
          |> fun b -> next_mark b mark_x
          |> fun i -> Printf.printf "%d" i;assert(8 = i); chgmark b i mark_x
          |> fun b -> chgmark b 4 mark_o
          |> fun b -> next_mark b mark_x
          |> fun i -> assert(7 = i); chgmark b i mark_x
          |> fun b -> chgmark b 6 mark_o
          |> fun b -> next_mark b mark_x
          |> fun i -> Printf.printf "%d" i; assert(2 = i); chgmark b i mark_x
          |> fun b -> chgmark b 3 mark_o
          |> fun b -> assert(mark_o = winner b);
    Printf.printf("test game done\n")
let _ = test_game ()
