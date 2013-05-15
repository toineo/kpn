(*** A continuation monad, written in the most general way possible
     Inspired by Haskell's Control.Monad.Trans.Cont ***)

(*open Operators*)

let identity x = x
let const x y = x (* the K combinator *)

(* Parameters of the continuation type :
   'a : type of the value produced by this step of the computation
   'b : final type of the entire computation
        i.e. return type of the current continuation
   For a fixed b
*)
type ('a,'b) t = Cont of (('a -> 'b) -> 'b)

(* Naming conventions :
   k = continuation
   c = computation with access to the current continuation
   x = value
   f = function / Kleisli morphism of type 'a -> ('b,'c) Continuation.t
*)

let runCont (Cont c) k = c k (* k = final continuation *)
    
let return x = Cont (fun k -> k x)

let bind (Cont c) f = Cont (fun k -> c (fun x -> runCont (f x) k))
let (>>=) = bind

let callCC f = Cont (fun k -> let escape x = Cont (fun _ -> k x) in
                              runCont (f escape) k)


(** Examples of continuation usage **)

let list_product_cont l =
  let rec f escape = function
    | [] -> return 1
    | x::xs -> if x = 0 then escape 0
               else f escape xs >>= fun prod -> return (x * prod)
  in
  let computation =
    callCC (fun escape ->
              f escape l >>= fun x ->
              print_endline "normal exit";
              return x) 
    >>= fun x ->
    print_endline "always displayed";
    return x
  in
  runCont computation identity


(* 
module Kahn : Kahn.S = struct
  type 'a process = undef

  type 'a channel = 'a Queue.t
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let new_channel () = Queue.create ()

  let put v c () =
    Queue.push v c.q;
    Coroutine.yield ()

  let rec get c () =
    try
      Queue.pop c.q
    with Queue.Empty ->
      Coroutine.yield ();
      get c ()

  let doco l () =
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths

  let return v = (fun () -> v)

  let bind e e' () =
    let v = e () in
    Thread.yield ();
    e' v ()

  let run e = e ()

end
*)
