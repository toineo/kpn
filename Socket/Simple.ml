(*** Basic prototype gluing Client and Server together
     to have 2 processes communicating over localhost
***)

open Operators

let server_port = 10000
let local_ip = Server.get_addr ()

module Cfg = struct
  let ip = local_ip
  let port = server_port 
end

module Srv = Server.SimpleServer (Cfg)
module Cl = Client.Client (Cfg)

let next_id = ref 0

module Kahn : KPN.S = struct include Cl
  let bind = bind

  let run e = 
    ignore $ Thread.create Srv.run ();    (* no need for thread id ? *)
    let () = Thread.yield ()  in (* Launch the server *)
    let () = Thread.delay 0.1 in (* FIXME : ? *)
    Cl.run e

  let new_channel () = 
    let id = !next_id in
    incr next_id;
    open_channel id
end
