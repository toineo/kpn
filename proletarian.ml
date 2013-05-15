(*** Horribly overengineered implementation of Kahn Process Networks
     using a sequential implementation of concurrency adapted from
     "A poor man's concurrency monad"

     Instead of defining a generic monad transformer, we just take as
     a "base monad" the type constructor 'a => (unit -> 'a)
     (which is basically something like the IO monad in an impure language)

     The "par" and "fork" combinators are replaced by a fork_join operation
     which spawns processes and waits for them to end
***)

open Operators

module C = Continuation

let return = C.return
let bind = C.bind
let (>>=) = bind


type action = Atom of (unit -> action)
            | ForkJoin of action list * (unit -> action)
            | Stop

type 'a process = ('a, action) Continuation.t

(* Turn continuation into action *)

let action c = C.run_cont c (fun _ -> Stop)

(* Building actions *)

let atom f = C.cont (fun k -> let x = f () in
                              Atom (fun () -> k x) )

let stop = C.cont (fun _ -> Stop)

let fork_join cs = C.cont (fun k -> ForkJoin (List.map action cs, k))


(* Scheduling *)

(* assign each fake thread a PID and track them so that parent threads
   wait for their children *)

module S = Set.Make(struct
  type t = int
  let compare = Pervasives.compare
end)

type fake_thread_status = ThreadExec of action
                          | ThreadWait of (unit -> action)
type fake_thread = fake_thread_status * int * int (* parent pid and self pid *)
  
let run start_process =
  
  (* simple hack to get the return value *)
  let retval = ref None in
  let start_action = C.run_cont start_process
                                (fun x -> retval := Some x; Stop)
  in

  let queue = Queue.create () in
  let push x = Queue.push x queue in
  push (ThreadExec start_action, -1, 0);
  let observed_parent_pids = ref S.empty in
  let next_pid = ref 1 in

  while not (Queue.is_empty queue) do
    let (thread_status, parent_pid, self_pid) = Queue.pop queue in
    if thread_status <> ThreadExec Stop
    then observed_parent_pids := S.add parent_pid !observed_parent_pids;
    match thread_status with
    | ThreadExec act -> (match act with
      | Atom f -> push (ThreadExec (f ()), parent_pid, self_pid)
      | ForkJoin (actions, k) ->
        List.iter (fun act' ->
                     push (ThreadExec act', self_pid, !next_pid);
                     incr next_pid)
                  actions;
        push (ThreadWait k, parent_pid, self_pid)
      | Stop -> ()
    )
    | ThreadWait k ->
        if S.mem self_pid !observed_parent_pids
          (* reinitialize for another round *)
        then observed_parent_pids := S.remove self_pid !observed_parent_pids
          (* all children finished : resume continuation! *)
        else (observed_parent_pids := S.remove self_pid !observed_parent_pids;
              push (ThreadExec (k ()), parent_pid, self_pid) )
  done;

  let (Some v) = !retval in v



(** Implement Kahn interface **)
			 
(* channels = simple queues
   no locks necessary : we can get atomicity guarantees from the framework *)
			 
type 'a in_port = 'a Queue.t
type 'a out_port = 'a Queue.t
  
let new_channel () = let q = Queue.create () in (q,q)
let put x q = atom (fun () -> Queue.push x q)
let get q = atom (fun () -> Queue.pop q)

let doco = fork_join
