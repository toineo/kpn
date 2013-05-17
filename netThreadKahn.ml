module Kahn : Kahn.S = struct
  open Operators
  open Thread
  open Unix

  let server_port = 10000
  let local_ip = 
    (* inet_addr_of_string "127.0.0.1" *)
    ( Unix.gethostbyname "localhost" ).Unix.h_addr_list.(0)
  let serv_addr = ADDR_INET (local_ip, server_port)

  type 'a process = (unit -> 'a)

  type ('a, 'b) channel = { id : int ; ch : 'b }
  type 'a in_port = ('a, in_channel * out_channel) channel
  type 'a out_port = ('a, out_channel) channel

  let fire = create

  let server_socket = handle_unix_error 
    (fun () -> socket PF_INET SOCK_STREAM(* SEQPACKET *) 0) ()
  let next_id = ref 0

  (** Request/objects queue **)
  type slot = 
    | Obj of string
    | Query of (string -> unit)

  type queue_state =
    (* FIXME : choose better names *)
    | Empty
    | Normal 
    | Request

  let objects : (int, slot Queue.t) Hashtbl.t = Hashtbl.create 577
  let obj_mutex = Mutex.create ()


  (* Request/objects queue utility functions *)
  (* /!\ This function lock the hashtable mutex /!\ *)
  let get_fifo id =
    Mutex.lock obj_mutex;
    try Hashtbl.find objects id 
    with
    | Not_found ->
      let fifo = Queue.create () in
      Hashtbl.add objects id fifo;
      fifo
      
  let get_fifo_state q =
    if Queue.is_empty q then
      Empty

    else
      match Queue.peek q with
        | Obj _ -> Normal
	| Query _ -> Request
      

  let rec server_worker fd =
    let in_ch = in_channel_of_descr fd in
    while true do 
      let id = input_binary_int in_ch in
      let op = input_line in_ch in
      match op with
	| "put" ->
	  let obj = input_line in_ch in
	  let fifo = get_fifo id in
	  begin
	    match get_fifo_state fifo with
	      | Empty | Normal ->
		Queue.add (Obj obj) fifo;
		Mutex.unlock obj_mutex

	      | Request ->
		begin
		  match Queue.pop fifo with
	            | Query f -> f obj
		    | _ -> assert false
		end;

		Mutex.unlock obj_mutex
	  end
	    
	| "get" ->
	  let fifo = get_fifo id in
	  let obj = ref "" in
	  
	  (* Get the requested object and lock ourselves if not yet present *)
	  begin
	    match get_fifo_state fifo with
              | Normal -> 
		obj := (match Queue.pop fifo with
		  | Obj s -> s
		  | _ -> assert false)
		;
		Mutex.unlock obj_mutex

	      | Empty | Request ->
		let mut = Mutex.create () in
		let query = fun s -> obj := s; Mutex.unlock mut in
		Mutex.lock mut;
		Queue.add (Query query) fifo;

		(* FIXME : we should not be interrupted between next 2 instructions *)
		Mutex.unlock obj_mutex;
		Mutex.lock mut (* Let's purposefully lock ourselves ! *)
	  end;
	  
	  (* Then send the result *)
	  let out_ch = out_channel_of_descr fd in
	  output_string out_ch (!obj ^ "\n");
	  flush out_ch

	| _ -> assert false
    done

    
  let server_main () =
    handle_unix_error (fun () -> 
      bind server_socket serv_addr;
      listen server_socket server_port;) ();
    while true do 
      let fd = fst =< accept $ server_socket in
      ignore (fire server_worker fd) (* FIXME : make a real shutdown *)
    done


  (* References *)
  let server_tid = fire server_main ()

  let () = yield () (* Launch the server *)
  let () = delay 0.1 (* FIXME : ? *)

  
  let new_channel () =
    let id = !next_id in
    let make = fun conv -> 
      let sock = socket PF_INET SOCK_STREAM 0 in
      handle_unix_error (fun () -> connect sock serv_addr) ();
      { id = id; ch = conv sock } 
    in
    incr next_id;
    make (in_channel_of_descr &&& out_channel_of_descr), 
    make out_channel_of_descr


  let put (v : 'a) (c : 'a out_port) () =
    output_binary_int c.ch c.id;
    flush c.ch;
    output_string c.ch "put\n";
    flush c.ch;
    output_string c.ch =< (^) *- "\n" =< String.escaped 
    $ Marshal.to_string v [Marshal.Closures];
    flush c.ch;
    Thread.yield ()


  let rec get (c : 'a in_port) () =
    let in_ch, out_ch = c.ch in
    output_binary_int out_ch c.id;
    flush out_ch;
    output_string out_ch "get\n";
    flush out_ch;
    let ans = 
      Marshal.from_string *- 0 =< Scanf.unescaped =< input_line $ in_ch
    in
    Thread.yield ();
    ans


  let doco l () =
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths


  let return v = (fun () -> v)


  let bind e e' () =
    let v = e () in
    Thread.yield ();
    e' v ()


  let run e = e ()
end

