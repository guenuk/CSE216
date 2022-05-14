(*wire type*)
type wireInferface = {
    (*accessors*)
    get_signal: unit -> int; 
    set_signal: int -> unit;
    
    (*regiser callback*)
    add_action: (unit -> unit) -> unit
}

(*priority queue type*)
type 'a priorityQueueInterface = {
    (*number of entries*)
    size: unit -> int; 

    (*add a new entry (action due time, action)*)
    add: 'a -> unit;
    
    (*remove the first entry*)
    remove: unit -> 'a 
}

(*simulator type*)
type simulatorInterface = {
    (*add delay and action callback to pq*)
    after_delay: int -> (unit -> unit) -> unit; 
    
    (*call the first action callback in pq*)
    propagate: unit -> unit;
    
    (*current simulation time*)
    now: unit -> int
}

(*
   priority queue object
*)
let make_priority_queue default_elem =
    (*local fields*)
    let data = ref (Array.make 10 default_elem) in
    let size = ref 0 in
    let key (k,_) = k in
    
    (*resize if necessary*)
    let try_resize () = (*dynamic array*)
        if !size + 1 >= Array.length !data then begin    
            let data' = Array.init
                            (!size * 2 )
                            (fun i -> if i < !size
                                      then !data.(i)
                                      else default_elem) in
            data := data'
        end
        else () in

    (*shift the elements to the left from*)
    let shift_left from =
        let rec iter i =
            if i < !size then begin
                !data.(i - 1) <- !data.(i);
                iter (i + 1)
            end
            else () in
        iter from;
        size := !size - 1 in
        
    (*shift the elements to the right from*)
    let shift_right from =
        let rec iter i =
            if i >= from then begin
                !data.(i + 1) <- !data.(i);
                iter (i - 1)
            end
            else () in
        iter (!size - 1);
        size := !size + 1 in

    (*add element*)
    let add elem =
        let rec try_insert i =
            if i = !size || key elem < key !data.(i) then begin
                shift_right i;
                !data.(i) <- elem
            end
            else 
                try_insert (i + 1) in
        try_resize ();
        try_insert 0 in
        
    (*remove element*)
    let remove elem =
        let min = !data.(0) in
        shift_left 1;
        min in

    (*return the priority queue interface*)
    let priority_queue : 'a priorityQueueInterface = {
        add    = add;
        remove = remove;
        size   = fun () -> !size
    } in
    priority_queue

(*priority queue test code*)    
let priority_queue_test () = 
    let value (_,v) = v in
    let pq = make_priority_queue (0,0) in
    let rec iter () =
        if pq.size () > 0 then begin
            Printf.printf "%d\n" (value (pq.remove ()));
            iter ()
        end
        else () in
    pq.add (9,9); pq.add (7,7); pq.add (2,2);
    pq.add (1,1); pq.add (3,3); pq.add (4,4);
    pq.add (6,6); pq.add (5,5); pq.add (8,8);
    pq.add (0,0); pq.add (9,11); pq.add (7,10); 
    iter ()
   
(*
   simulator object
*)
let make_simulator () =
    (*local fields*)
    let now = ref 0 in (*the current time*)
    let pq = make_priority_queue (!now, fun () -> ()) in

    (*add action to be taken after delay from now*)
    let after_delay delay action = 
        pq.add (!now + delay, action) in
        
    (*until pq is emtpy, simulate: propagate actions*)
    let rec propagate () = 
        if pq.size () > 0 then begin
            let (t, action) = pq.remove () in
            now := t;    (*advance the simulation time*)
            action ();   (*invoke the callback*)
            propagate () (*keep going until the queue is empty*)
        end
        else () in
        
    (*return the simulator interface*)
    let simulator : simulatorInterface = {
        after_delay = after_delay;
        propagate = propagate;
        now = fun () -> !now
    } in
    simulator

(*
   wire object
*)
let make_wire () = 
    (*local fields*)
    let signal_value = ref 0 in
    let callbacks = ref [] in
    
    (*set signal to new value:
      if there is a change, notify the callbacks
    *)
    let set_signal new_value = 
        if !signal_value <> new_value then begin
            signal_value := new_value;
            List.iter (fun f -> f ()) !callbacks
        end
        else () in
        
    (*register callback procedures*)
    let add_action proc =
        callbacks := proc :: !callbacks;
        proc () in
        
    (*return the wire interface*)
    let wire : wireInferface = {
        get_signal = (fun () -> !signal_value);
        set_signal = set_signal;
        add_action = add_action
    } in
    wire

(*
   probe
*)    
let probe sim wire name =
    wire.add_action
        (fun () -> Printf.printf
                    "[t:%d] %s: %d\n"
                    (sim.now ())
                    name
                    (wire.get_signal ()))

(*
   not gate
*)    
let not_gate sim input output =
    let logical_not a = if a = 1 then 0 else 1 in
    let not_gate_delay  = 10 in
    (*callback*)
    let not_action () =
        (*on change in input wire, compute the new value*)
        let new_value = logical_not (input.get_signal ()) in
        (*after the delay, set the new value to output wire*)
        sim.after_delay not_gate_delay
                    (fun () -> output.set_signal new_value) in
                    
    (*register callback to input wire*)
    input.add_action not_action

(*
   and gate
*)    
let and_gate sim a1 a2 output =
    let logical_and a b = if a = 1 && b = 1 then 1 else 0 in
    let and_gate_delay = 12 in
    (*callback*)
    let and_action () =
        (*on change in input wires, compute the new value*)
        let new_value = logical_and (a1.get_signal ())
                                    (a2.get_signal ()) in
        (*after the delay, set the new value to output wire*)
        sim.after_delay and_gate_delay
                    (fun () -> output.set_signal new_value) in
                    
    (*register callback to input wires*)
    a1.add_action and_action;
    a2.add_action and_action
    
(*
   or gate
*)    
let or_gate sim a1 a2 output =
    let logical_or a b = if a = 1 || b = 1 then 1 else 0 in
    let or_gate_delay = 13 in
    (*callback*)
    let or_action () =
        (*on change in input wires, compute the new value*)
        let new_value = logical_or (a1.get_signal ())
                                   (a2.get_signal ()) in
        (*after the delay, set the new value to output wire*)
        sim.after_delay or_gate_delay
                    (fun () -> output.set_signal new_value) in
                    
    (*register callback to input wires*)
    a1.add_action or_action;
    a2.add_action or_action

(*
   half adder
*)
let half_adder sim a b s c = 
    let d = make_wire () in
    let e = make_wire () in
     or_gate sim a b d;
    and_gate sim a b c;
    not_gate sim c e;
    and_gate sim d e s

(*
   full adder
*)
let full_adder sim a b cin sum cout =   
    let s  = make_wire () in
    let c1 = make_wire () in
    let c2 = make_wire () in
    half_adder sim b cin s c1;
    half_adder sim a s sum c2;
       or_gate sim c1 c2 cout

(*
   simulation
*)    
let test () = 
    (* let _ = priority_queue_test () in *)
    let sim  = make_simulator () in
    
    (*wires*)
    let a    = make_wire () in    
    let b    = make_wire () in    
    let cin  = make_wire () in    
    let sum  = make_wire () in    
    let cout = make_wire () in

    (*full adder*)
    full_adder sim a b cin sum cout;
    
    (*initial config*)
    a.set_signal 1;
    b.set_signal 1;
    cin.set_signal 1;

    (*attach probes*)
    probe sim a "a";
    probe sim b "b";
    probe sim sum "sum";
    probe sim cin "cin";
    probe sim cout "cout";
    
    (*run the simulation*)
    sim.propagate ();
    
    (*final result*)
    (sum.get_signal (), cout.get_signal ())

let _ = test ()
