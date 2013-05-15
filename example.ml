module Example (K : Kahn.S) = struct
  module K = K
  module Lib = Lib.Lib(K)
  open Lib

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (* Format.printf "Firing %d@." n; *)
      (* FIXME : debug *)
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2

  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (* Format.printf "Waiting...@."; *)
      (* FIXME : debug *)
      (K.get qi) >>= (fun v -> Format.printf "%d@." v; loop ())
    in
    loop ()

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])

end

(* TODO : faire un exemple plus massif *)

module E = Example(ThreadKahn.Kahn)
(* module E = Example(TubesProcKahn.Kahn) *)
(* module E = Example(NetThreadKahn.Kahn) (\* Pas fonctionnel encore *\) *)
(* module E = Example(Proletarian) *)

let () = E.K.run E.main
