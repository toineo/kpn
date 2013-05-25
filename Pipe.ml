module Kahn : KPN.S = struct
  open Unix
  open Operators

  type 'a process = (unit -> 'a)

  type 'a in_port = in_channel
  type 'a out_port = out_channel

  let new_channel = 
    in_channel_of_descr *** out_channel_of_descr =< pipe 


  let put (v : 'a) 
      (p : 'a out_port)
      () =
    Marshal.to_channel p v [Marshal.Closures]
    (* flush p *)

  let rec get (p : 'a in_port) () =
    (Marshal.from_channel p : 'a)


  let doco l () =
    let procs = List.map 
      (fun f -> 
	match fork () with 
	  | 0 -> f (); exit 0
	  | pid -> pid
      ) l in
    List.iter (ignore =< waitpid []) procs

  let return v = fun () -> v

  let bind e e' () =
    let v = e () in
    e' v ()

  let run e = e ()
end

