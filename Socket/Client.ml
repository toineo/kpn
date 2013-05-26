module Client = functor (Cfg : Server.ServerConfig) -> struct
  open Operators
  open Unix

  type 'a process = (unit -> 'a)

  type ('a, 'b) channel = { id : int ; ch : 'b }
  type 'a in_port = ('a, in_channel * out_channel) channel
  type 'a out_port = ('a, out_channel) channel

  (** Request/objects queue **)
  module ROHstb = RequestObjectQueue.ObjReqHashtbl (struct type t = int end) (String)
  open ROHstb.Queue

  (* Request a connection with the channel number id on the server *)
  let open_channel id =
    let make = fun f -> 
      let sock = socket PF_INET SOCK_STREAM 0 in
      handle_unix_error (fun () -> connect sock $ ADDR_INET (Cfg.ip, Cfg.port)) ();
      { id = id; ch = f sock } 
    in
    make (in_channel_of_descr &&& out_channel_of_descr), 
    make out_channel_of_descr


  (* PUT request: write (channel id, "put", marshalled data) to socket *)
  let put (v : 'a) (c : 'a out_port) () =
    output_binary_int c.ch c.id;
    flush c.ch;
    output_string c.ch "put\n";
    flush c.ch;
    output_string c.ch =< (^) *- "\n" =< String.escaped
    $ Marshal.to_string v [Marshal.Closures];
    flush c.ch;
    Thread.yield ()

  (* GET request: write (channel id, "get");
                  blocking read for marshalled data *)
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

  (* Primitive doco which launches threads in the same client *)
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
