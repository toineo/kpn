(*** A continuation monad, written in the most general way possible
     Inspired by Haskell's Control.Monad.Trans.Cont
***)

open Monad
open Operators

let identity x = x
let const x y = x (* the K combinator *)

(* Parameters of the continuation type :
   'a : type of the value produced by this step of the computation
   'b : final type of the entire computation
        i.e. return type of the current continuation
   For a fixed 'b, 'a => ('a,'b) t is a monad.
*)
type ('a,'b) t = Cont of (('a -> 'b) -> 'b)

(* Naming conventions :
   k = continuation
   c = computation with access to the current continuation
   x = value
   f = function / Kleisli morphism of type 'a -> ('b,'c) Continuation.t
*)

(* for some stupid reason OCaml constructors cannot be used
   like regular functions
*)
let cont c = Cont c

let run_cont (Cont c) k = c k (* k = final continuation *)
    
let return x = cont $ fun k -> k x

let bind (Cont c) f = cont $ fun k -> c (fun x -> run_cont (f x) k)
let (>>=) = bind

(* Note the difference between the previous "cont" and "call_cc":
   "cont" takes as a parameter a function which takes a continuation,
   and whose responsibility it is to pass the result of its computation
   to the continuation; meanwhile, "call_cc" gives one an escape function
   which will allow one to call the continuation while remaining inside
   the Cont monad, and when this escape function is not explicitly used,
   the computation will resume normally instead of abruptly stopping there.

   Using call_cc in a big computation inside the Cont monad is akin to
   the usage of call/cc in Scheme: the continuations are undelimited,
   they represent the rest of the program.

   Using cont and run_cont, one has access to delimited continuations;
   for examples of their usage, see proletarian.ml and coroutine.ml
*)

let call_cc f = cont $ fun k -> let escape x = cont $ fun _ -> k x in
                               run_cont (f escape) k


(* just for fun: a monad transformer *)
module ContT (M : Monad) = struct
  type ('a,'b) t = ContT of (('a -> 'b M.t) -> 'b M.t)
  let cont_t c = ContT c
  let run_cont_t (ContT c) = c
  let return x = cont_t $ fun k -> k x
  let bind (ContT c) f = cont_t $ fun k -> c (fun x -> run_cont_t (f x) k)
  let call_cc f = cont_t $ fun k -> let escape x = cont_t $ fun _ -> k x in
                                    run_cont_t (f escape) k
end


(** Examples of call/cc usage **)

let list_product_cont l =
  let rec f escape = function
    | [] -> return 1
    | x::xs -> if x = 0 then escape 0
               else f escape xs >>= fun prod -> return (x * prod)
  in
  let computation =
    call_cc (fun escape ->
              f escape l >>= fun x ->
              print_endline "normal exit";
              return x) 
    >>= fun x ->
    print_endline "always displayed";
    return x
  in
  run_cont computation identity
