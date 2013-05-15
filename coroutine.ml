(*** Cooperative multitasking with coroutines !
     
     They are implemented using call/cc in the continuation monad
     from continuation.ml, with the environment carrying an additional
     information : the point in the coroutine's caller where it should resume
     upon being restored control by a call to "yield" in the callee.
     The coroutine monad would be ReaderT (ContT IO) in Haskell.

     Note that despite the ability for coroutines to return values,
     they will only return () in practice when used in the KPN
     implementation.
***)

open Operators

module C = Continuation

module Coroutine = struct

  (* mutually recursive data types !
     a coroutine remembers the continuation of its parents (the reader data)
     which also produces a coroutine *)

  type 'a ret_type = Yield  of 'a * (unit -> (unit,'a) t) (* resume point *)
                   | Return of 'a                         (* execution finished *)
  (* it is unfortunate that the "return" of imperative languages which
     concludes a subroutine is quite different from the "return" in
     a monad despite the 2 having the same name *)

  and 'a reader_data = 'a ret_type -> (unit,'a) t

  (* a step producing an intermediate result of type 'a
     in a coroutine whose return value has type 'b *)
  and ('a,'b) t = CR of ('b reader_data -> ('a, unit) Continuation.t)

  let run_reader (CR f) = f

  let cr_ x = CR x
  
  let return x = cr_ $ fun _ -> C.return x
  let bind r f = cr_ $ fun x -> C.bind (run_reader r x)
                                       (fun y -> run_reader (f y) x)
  let (>>=) = bind

  let ask = cr_ $ fun x -> C.return x
  let with_reader x m = cr_ $ fun _ -> run_reader m x
  let lift cont = cr_ $ fun _ -> cont

  (* callCC lifted to handle computations in the transformed monad
     see also liftCallCC in Control.Monad.Trans.Reader *)
  let callCC' f = cr_ $ fun x -> C.callCC $ fun k ->
    run_reader (f (cr_ =< C.const =< k)) x

  (* call a (child) coroutine *)
  let call r = callCC' $ fun ret_point -> 
    with_reader ret_point $ r >>= fun x -> return $ Return x

  (* return control to the parent *)
  let yield x = ask >>= fun ret_point ->
    callCC' $ fun k ->
      ret_point $ Yield (x, k)

  (* this execution function runs the coroutine until it finishes
     and only returns the last value produced *)
  let run coroutine =
    let rec loop r = call r >>= function
      | Yield (_, resume) -> loop (resume ())
      | Return value -> return value
    in
    let ret_val = ref None in
    C.run_cont *- (fun x -> ret_val := Some x)
    =< run_reader *- (fun _ -> assert false)
    $  loop coroutine;
    let (Some v) = !ret_val in v
    
end

open Coroutine

let generate_ints =
  let rec loop n = yield n >>= fun _ -> loop (n+1) in
  loop 0

let print_ints = call generate_ints >>= fun (Yield (n,resume)) ->
    print_int n; print_newline (); call (resume ())

let _ = run print_ints

(*
let return = Coroutine.return
let bind = Coroutine.bind

type 'a process = (unit,'a) Coroutine.t

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

*)
