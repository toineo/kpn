module Example (K : KPN.S) = struct
  module K = K
  module Lib = Lib.Lib(K)
  open Lib

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2

  let rec filter (qi : int K.in_port) : unit K.process =
    K.get qi >>= fun n ->
    let next_in, out = K.new_channel () in
    let rec loop () =
      K.get qi >>= fun v -> (if v mod n <> 0 then (K.put v out) else K.return ()) >>= fun () -> loop ()
    in
    Format.printf "%d@." n;
    K.doco [filter next_in; loop ()]
      
  let main : unit K.process =
    (delay K.new_channel ()) >>=
      (fun (q_in, q_out) -> K.doco [ integers q_out ; filter q_in ; ])
      
end

(* module E = Example(Kahn.Pipe) *)
(* module E = Example(Kahn.Socket) *)
module E = Example(Kahn.Seq)

let () = E.K.run E.main
