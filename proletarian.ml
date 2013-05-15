(*** Horribly overengineered mplementation of Kahn Process Networks
     using the concurrency framework from "A poor man's concurrency monad"

     Instead of defining a generic monad transformer, we just take as
     a "base monad" the type constructor 'a => (unit -> 'a)
     (which is basically something like the IO monad in an impure language)
***)


module Continuation = struct


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

let cont c = Cont c

let run_cont (Cont c) k = c k (* k = final continuation *)
    
let return x = Cont (fun k -> k x)

let bind (Cont c) f = Cont (fun k -> c (fun x -> run_cont (f x) k))
let (>>=) = bind

let callCC f = Cont (fun k -> let escape x = Cont (fun _ -> k x) in
                              run_cont (f escape) k)

end

module C = Continuation

let return = C.return
let bind = C.bind
let (>>=) = bind


type action = Atom of (unit -> action)
            | Fork of action * action
            | Stop

type 'a process = ('a, action) Continuation.t

(* GADTs for the win ! *)
(* type _ action = Atom : (unit -> 'a action) -> 'a action
              | Fork : unit action * unit action -> unit action
              | Result : 'a -> 'a action *)

(* Turn continuation into action *)

let action c = C.run_cont c (fun _ -> Stop)

(* Building actions *)

let atom f = C.cont (fun k -> let x = f () in
                              Atom (fun () -> k x) )

let stop = C.cont (fun _ -> Stop)

(* the "par" combinator is stupid and therefore not implemented here
   (threading the continuation to both halves results in the rest
   of the process being executed twice)
*)

let fork c = C.cont (fun k -> Fork (action c, k ()) )

(* Scheduling *)

let exec_parallel actions =
  let queue = Queue.create () in
  let get_todo () = Queue.pop queue 
  and do_later act = Queue.push act queue in
  List.iter do_later actions;
  while not (Queue.is_empty queue) do
    match get_todo () with
    | Atom f -> do_later (f ())
    | Fork (a, a') -> do_later a; do_later a';
    | Stop -> ()
  done

(** Implement Kahn interface **)

(* channels = simple queues
   no locks necessary : we can get atomicity guarantees from the framework *)

type 'a in_port = 'a Queue.t
type 'a out_port = 'a Queue.t

let new_channel () = let q = Queue.create () in (q,q)
let put x q = atom (fun () -> Queue.push x q)
let get q = atom (fun () -> Queue.pop q)

(* parallel execution : use forks *)

let doco = undefined

let run c = exec_parallel [action c]
