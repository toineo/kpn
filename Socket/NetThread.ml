module Kahn : KPN.S = struct
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

  let fire = create

  let next_id = ref 0

  (** Request/objects queue **)
  module ROHstb = RequestObjectQueue.ObjReqHashtbl (struct type t = int end) (String)
  open ROHstb.Queue

  module Server = Server.SimpleServer (struct 
    let ip = local_ip
    let port = server_port 
  end)


  (* References *)
  let server_tid = fire Server.run ()

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

