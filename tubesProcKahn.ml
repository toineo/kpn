module Kahn : Kahn.S = struct
  open Unix
  open Operators

  type 'a process = (unit -> 'a)

  (* TODO : cleaner tout ça *)
  (*  Le 'a ne sert qu'au typage, il n'est pas utilisé dans la structure *)
  type 'a in_chan = in_channel
  type 'a out_chan = out_channel
  type 'a in_port = 'a in_chan
  type 'a out_port = 'a out_chan

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

