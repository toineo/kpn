module Kahn : Kahn.S = struct
  open Operators
  open Thread
  open Unix

  let server_port = 10000
  let local_ip = 
    (* inet_addr_of_string "127.0.0.1" *)
    (* ( Unix.gethostbyname "localhost" ).Unix.h_addr_list.(0) *)
    Server.get_addr ()

  type 'a process = (unit -> 'a)

  type ('a, 'b) channel = { id : int ; ch : 'b }
  type 'a in_port = ('a, in_channel * out_channel) channel
  type 'a out_port = ('a, out_channel) channel

  let fire = create (* FIXME : is it used here ? *)

  let next_id = ref 0

  (** Request/objects queue **)
  module ROHstb = RequestObjectQueue.ObjReqHashtbl (struct type t = int end) (String)
  open ROHstb.Queue

  (* let obj_mutex = Mutex.create () *)

  (* Request/objects queue utility functions *)
  (* /!\ This function lock the hashtable mutex /!\ *)
  (* let get_queue id = *)
  (*   Mutex.lock obj_mutex; *)
  (*   ROHstb.get_queue id *)
      
  (* let rec server_worker fd = *)
  (*   let in_ch = in_channel_of_descr fd in *)
  (*   while true do  *)
  (*     let id = input_binary_int in_ch in *)
  (*     let op = input_line in_ch in *)
  (*     let fifo = get_queue id in *)
  (*     match op with *)
  (*       | "put" -> *)
  (*         let obj = input_line in_ch in *)
  (*         begin *)
  (*           match get_state fifo with *)
  (*             | Empty | Normal -> *)
  (*       	add (Obj obj) fifo; *)
  (*       	Mutex.unlock obj_mutex *)

  (*             | Request -> *)
  (*       	begin *)
  (*       	  match pop fifo with *)
  (*                   | Query f -> f obj *)
  (*       	    | _ -> assert false *)
  (*       	end; *)

  (*       	Mutex.unlock obj_mutex *)
  (*         end *)
	    
  (*       | "get" -> *)
  (*         let obj = ref "" in *)
	  
  (*         (\* Get the requested object and lock ourselves if not yet present *\) *)
  (*         begin *)
  (*           match get_state fifo with *)
  (*             | Normal ->  *)
  (*       	obj := (match pop fifo with *)
  (*       	  | Obj s -> s *)
  (*       	  | _ -> assert false) *)
  (*       	; *)
  (*       	Mutex.unlock obj_mutex *)

  (*             | Empty | Request -> *)
  (*       	let mut = Mutex.create () in *)
  (*       	let query = fun s -> obj := s; Mutex.unlock mut in *)
  (*       	Mutex.lock mut; *)
  (*       	add (Query query) fifo; *)

  (*       	(\* FIXME : we should not be interrupted between next 2 instructions *\) *)
  (*       	Mutex.unlock obj_mutex; *)
  (*       	Mutex.lock mut (\* Let's purposefully lock ourselves ! *\) *)
  (*         end; *)
	  
  (*         (\* Then send the result *\) *)
  (*         let out_ch = out_channel_of_descr fd in *)
  (*         output_string out_ch $ !obj ^ "\n"; *)
  (*         flush out_ch *)

  (*       | _ -> assert false *)
  (*   done *)

    
  (* let server_main () = *)
  (*   handle_unix_error (fun () ->  *)
  (*     bind server_socket serv_addr; *)
  (*     listen server_socket server_port;) (); *)
  (*   while true do  *)
  (*     let fd = fst =< accept $ server_socket in *)
  (*     ignore (fire server_worker fd) (\* FIXME : make a real shutdown *\) *)
  (*   done *)

  module Server = Server.SimpleServer (struct 
    let ip = local_ip
    let port = server_port 
  end)


  (* References *)
  let server_tid = Server.run
    (* fire server_main () *)

  let () = yield () (* Launch the server *)
  let () = delay 0.1 (* FIXME : ? *)

  
  let new_channel () =
    let id = !next_id in
    let make = fun conv -> 
      let sock = socket PF_INET SOCK_STREAM 0 in
      handle_unix_error (fun () -> connect sock $ Server.get_addr ()) ();
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
    let ths = List.map (Thread.create *- ()) l in
    List.iter Thread.join ths


  let return v = (fun () -> v)


  let bind e e' () =
    let v = e () in
    Thread.yield ();
    e' v ()


  let run e = e ()
end

