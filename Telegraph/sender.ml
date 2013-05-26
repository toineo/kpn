module Cl = Socket.Client.Client(Cfg)
open Cl

let (_, out_ch) = open_channel 1

let rec loop () =
  bind (put (read_line ()) out_ch) (fun () -> print_endline "foo"; loop ())

let _ = run (loop ())
