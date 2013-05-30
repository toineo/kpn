module Cl = Socket.Client.Client(Cfg)
open Cl

let (_, out_ch) = open_channel 1

let rec loop n =
  bind (put n out_ch) 
    (fun () -> Thread.delay 1.; loop (n+1))
  (* bind (put (read_line ()) out_ch) (fun () -> print_endline "foo"; loop ()) *)

let _ = run (loop 1)
