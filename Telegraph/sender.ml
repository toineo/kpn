module Cl = Socket.Client.Client(Cfg)
open Cl

let (_, out_ch) = open_channel 1

let rec loop () =
  bind (put (read_line ()) out_ch) loop

let _ = run (loop ())
