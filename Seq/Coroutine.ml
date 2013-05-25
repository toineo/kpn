(*** Cooperative multitasking with coroutines ! ***)

open Operators

(** These coroutines can exit (with a return value) and resume multiple
    times but they always return the control to the caller (that is,
    they can't choose who to yield to).

    They are implemented using delimited continuations provided by the
    continuation.ml module. Coroutines are represented as computations
    in the Cont monad; within a coroutine, a yield operation can, by 
    calling "cont", access the rest of the computation occurring
    *inside the coroutine*, and suspend it for future use.

    See also "Yield: Mainstream Delimited Continuations", although
    the type proposed here differs in some respects: first, a coroutine
    doesn't receive new input when resumed; secondly, the paper's
    implementation considers the return type of a computation to be the
    type produced by the intermediate computations, whereas we take
    these two as separate while requiring runnable coroutines to return
    the same type that they yield.
**)

module C = Continuation

module Coroutine = struct
  (* ('a,'b) partial : piece of a computation whose final return type is 'b,
     producing an intermediate value of type 'a which it passes to
     the continuation
     'b t : complete coroutine which returns the type 'b
     'b return_type : the sum type passed to the caller when yielding
     
     with our choice of type variables, we avoid the explicit type
     quantifiers present in the paper (which would not fit neatly into
     OCaml's rank-1 polymorphism), but we have to introduce mutually
     recursive type declarations
  *)
  type ('a,'b) partial = CR of ('a, 'b return_type) C.t
  (* type ('a,'b) partial = ('a, 'b return_type) C.t *)
  and 'b t = ('b, 'b) partial
  and 'b return_type = Yield of 'b * (unit -> 'b return_type)
                       (* returned value * continuation *)
                     | Return of 'b

  (* A previous version of the following massively used the utilities in
     Operators.ml to practice "point-free style" i.e. avoiding the naming
     of variables when possible, but it turns out this results in errors
     such as
     Error: The type of this expression,
            '_a return_type -> ('_a return_type, '_b) partial,
             contains type variables that cannot be generalized
     Actually, one can often run into this kind of trouble when using
     this module...
  *)
  let cr_ x = CR x
  let uncr_ (CR x) = x

  (* monadic interface: just re-export the functions from Cont
     with some (un)wrapping
  *)
  let return x = cr_ (C.return x)
  let bind m f = cr_ (C.bind (uncr_ m) (fun x -> uncr_ (f x)))
  let (>>=) = bind

  (* coroutine-specific functions
     (use "call_" from outside a coroutine computation, "call" from inside)
  *)
  let call_ (CR r) = C.run_cont r $ fun x -> Return x
  let call r = return (call_ r)
  (* call_and_freeze defined such that resume (call_and_freeze r) = call r *)
  let call_and_freeze r () = call_ r
  let resume k = return (k ())
  let yield x = cr_ (C.cont $ fun k -> Yield (x, k))

  (* throw away all values but the last *)
  let run r =
    let rec loop = function
      | Yield (_, k) -> loop (k ())
      | Return v -> v
    in
    loop (call_ r)
end

(** Example of coroutine use: an integer-listing program
    analogous to the example for KPNs **)

module CoroutineExample = struct

  open Coroutine

  let generate_ints =
    let rec loop n = yield n >>= fun () -> loop (n+1) in
    loop 0

  let rec print_ints =
    let rec loop (Yield (n,next)) =
      print_int n; print_newline ();
      resume next >>= loop
    in
    call generate_ints >>= fun x ->
    loop x >>= fun _ ->
    return () (* necessary to constrain the return type
                 and avoid the dreaded type-variable-generalization problem *)

  let main () = run print_ints

end

(** Now we turn to the implementation of KPNs.

    A process is just a coroutine with final return type unit, where yielding
    is used solely to transfer control, not values. (This allows us to sidestep
    the aforementioned type-variable-generalization issues.)
    We redefine the sequencing operator bind to add yielding between 2 steps
    of the computation, so that the control may alternate between the different
    threads.

    A lot of the work has already been done above, the only things remaining are:
    * channels: easy, since everything runs sequentially, we can arrange for
      reading/writing on queues to be atomic and we don't need locks!
    * parallel execution: when a process spawns concurrent children, it takes
      responsibility for their management by making them run in a round-robin
      scheduling is not global, but done at each level of forking
**)    

module Kahn : KPN.S = struct
  module R = Coroutine

  type 'a process = ('a, unit) R.partial

  let return = R.return
  let bind m f = let (>>=) = R.bind in
                 m >>= fun x -> R.yield () >>= fun () -> f x
  let (>>=) = bind
  let yield_then_do f = return () >>= f

  type 'a in_port = 'a Queue.t
  type 'a out_port = 'a Queue.t

  let new_channel () = let q = Queue.create () in (q,q)
  let put x q = return (Queue.push x q)
  let rec get q = try return (Queue.pop q)
                  with Queue.Empty -> R.yield () >>= fun () -> get q


  let run proc = let retval = ref None in
                 R.run (R.bind proc (fun x -> retval := Some x; return ()));
                 let (Some v) = !retval in v

  (* Trick to ensure fair time sharing: in doco, the process does one
     entire round without yielding to the parent using R.bind instead
     of bind. That way processes closer to the top of the process tree
     do not get more time.
  *)
  let doco proc_list =
    let rec loop enq deq = match (enq, deq) with
      | [], [] -> return ()
      | _ , [] -> yield_then_do $ fun () -> loop [] (List.rev enq)
      | _ , k::ks -> R.bind (R.resume k) (function
        | R.Yield ((), next) -> loop (next::enq) ks
        | R.Return ()        -> loop enq deq)
    in
    loop (List.rev_map R.call_and_freeze proc_list) []

end
