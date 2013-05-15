(*** Monads ! ***)

(** Type signature **)

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
end

(** Monad examples **)

(* Does nothing *)

module IdentityM = struct
  type 'a t = 'a
  let return x = x
  let bind x f = f x
end

(* Monad of delayed side-effectful computations *)

module FoobarM = struct
  type 'a t = unit -> 'a
  let return x = fun () -> x
  let bind m f = fun () -> f (m ()) ()
end

(** Some generic functions on monads **)

module MonadUtil (M : Monad) = struct
  open M
  let (>>=) = bind
  let fmap f m = m >>= (fun x -> return (f x))
  let (<$>) = fmap
  let (<*>) mf mx = mf >>= fun f -> mx >>= fun x -> return (f x)
end
