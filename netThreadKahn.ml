module Kahn : Kahn.S = struct
  open Operators
  open Thread

  type 'a process = (unit -> 'a)

  type 'a channel = { id : int }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let fire = create

  (* References *)
  (* TODO : les protéger (mutex) *)
  let server_tid = ref (self ())
  let next_id = ref 0

  let rec server_main () =
    delay 1.;
    server_main ()

  let check_server () =
    if self () = !server_tid then
      server_tid := fire server_main () 

  let new_channel () =
    check_server ();
    let ch = {id = !next_id} in
    incr next_id;
    ch, ch

  let put v c () =
    (* TODO *)
    Thread.yield ()

  let rec get c () =
    (* TODO *)
    Thread.yield ()


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

