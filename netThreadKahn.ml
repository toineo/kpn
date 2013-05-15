module Kahn : Kahn.S = struct
  open Operators
  open Thread
  open Unix

  let server_port = 9999
  let local_ip = inet_addr_of_string "127.0.0.1" (* FIXME *)
  let serv_addr = ADDR_INET (local_ip, server_port)

  type 'a process = (unit -> 'a)

  type ('a, 'b) channel = { id : int ; ch : 'b }
  type 'a in_port = ('a, in_channel * out_channel) channel
  type 'a out_port = ('a, out_channel) channel

  let fire = create

  let server_socket = handle_unix_error 
    (fun () -> socket PF_INET SOCK_STREAM(* SEQPACKET *) 0) ()
  let next_id = ref 0

  let objects = Hashtbl.create 577

  type slot = 
    | Obj of string
    | Query of (string -> unit)

  let rec server_worker fd =
    let in_ch = in_channel_of_descr fd in
    while true do 
      let id = input_binary_int in_ch in
      let op = input_line in_ch in
      match op with
	| "put" ->
	  let obj = input_line in_ch in
	  begin
	    try let fifo = Hashtbl.find objects id in
		Queue.add obj fifo
	    with
	      | Not_found ->
		let fifo = Queue.create () in
		Queue.add obj fifo;
		Hashtbl.add objects id fifo
	  end
	| "get" ->
      (* TODO *)
      (* Format.printf "Id : %d - Op : %s - Obj : %s@." id op obj *)
    done
    
  let rec server_main () =
    Format.printf "Running@.";
    bind server_socket serv_addr;
    listen server_socket server_port;
    while true do 
      let fd = fst =< accept $ server_socket in
      fire server_worker fd;
      server_main ()
    done

  (* References *)
  (* TODO : les protéger (mutex) *)
  let server_tid = fire server_main ()

  let () = yield () (* Launch the server *)
  let () = delay 0.1 (* FIXME *)

  
  let new_channel () =
    let id = !next_id in
    let make = fun conv -> 
      let sock = socket PF_INET SOCK_STREAM 0 in
      handle_unix_error (fun () -> connect sock serv_addr) ();
      (* connect sock (getsockname server_socket); *)
      { id = id; ch = conv sock } 
    in
    incr next_id;
    (* TODO : mutex *)
    make (in_channel_of_descr &&& out_channel_of_descr), 
    make out_channel_of_descr

  let put (v : 'a) (c : 'a out_port) () =
    (* output_string c.ch (((^) *- "\n") =< string_of_int $ c.id); *)
    output_binary_int c.ch c.id;
    flush c.ch;
    output_string c.ch "put\n";
    flush c.ch;
    (* Marshal.to_channel c.ch v [Marshal.Closures]; *)
    output_string c.ch =< (^) *- "\n" =< String.escaped $ Marshal.to_string v [Marshal.Closures];
    flush c.ch;
    Thread.yield ()

  let rec get (c : 'a in_port) () =
    output_binary_int (snd c.ch) c.id;
    flush (snd c.ch);
    output_string (snd c.ch) "get\n";
    flush (snd c.ch);
    let ans = (Marshal.from_channel (fst c.ch) : 'a) in
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

