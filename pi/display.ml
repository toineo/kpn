module Cl = Socket.Client.Client(Cfg)
open Cl

let nb_chan = 3

let in_chan = (Array.make nb_chan (Obj.magic ()) : int in_port array) 
  (* FIXME : hack to force typing *)

let () =
  for i = 0 to nb_chan - 1 do
    in_chan.(i) <- fst (open_channel i)
  done;
  ()

let rec loop i =
  bind (get in_chan.(i)) (fun n -> Printf.printf "%x" n; loop ((i + 1) mod nb_chan))

let _ = run (loop 0)
