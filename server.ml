module type ServerConfig = sig
  val ip : int
  val port : int
end

module type Server = 
  functor (Cfg : ServerConfig) ->
    sig
      val run : unit -> unit
    end


module SimpleServer : Server = 
  functor (Cfg : ServerConfig) ->
    struct
      open Thread

      let serv_addr = ADDR_INET (local_ip, server_port)
	
      (* Utilities *)
      let fire = create

      (** Request/objects queue **)
      module ROHstb = RequestObjectQueue.ObjReqHashtbl (struct type t = int end) (String)
      open ROHstb.Queue

      let obj_mutex = Mutex.create ()

      (* Request/objects queue utility functions *)
      (* /!\ This function lock the hashtable mutex /!\ *)
      let get_queue id =
        Mutex.lock obj_mutex;
        ROHstb.get_queue id


	
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

    let run = server_main (* FIXME : do we really want the same behavior ? *)
end
