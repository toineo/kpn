module Cl = Socket.Client.Client(Cfg)
open Cl

let (in_ch, _) = open_channel 1

let rec loop () =
  bind (get in_ch) (fun s -> print_endline s; loop ())

let _ = run (loop ())
