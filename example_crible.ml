module Example (K : Kahn.S) = struct
  module K = K
  module Lib = Lib.Lib(K)
  open Lib

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      Format.printf "Firing %d@." n;
      (* FIXME : debug *)
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2

  let rec filter (qi : int K.in_port) : unit K.process =
    Format.printf "Waitin'@."; 	(* FIXME : debug *)
    let n = K.run (K.get qi) in
    let next_in, out = K.new_channel () in
    let rec loop () =
      (K.get qi) >>= (fun v -> (if v mod n <> 0 then K.run (K.put v out)); loop ())
    in
    Format.printf "%d@." n;
    K.doco [filter next_in; loop ()]
      
  let main : unit K.process =
    (delay K.new_channel ()) >>=
      (fun (q_in, q_out) -> K.doco [ integers q_out ; filter q_in ; ])
      
end


module E = Example(ThreadKahn.Kahn)
(* module E = Example(TubesProcKahn.Kahn) *)
(* module E = Example(NetThreadKahn.Kahn) (\* Pas fonctionnel encore *\) *)

let () = E.K.run E.main
