(* FIXME : les symboles ne sont pas forcément les mieux choisis... *)
(* "Pair pipin'" *)
let ( *** ) f g = fun (a, b) -> f a, g b

(* Composition *)
let ( =< ) f g = fun x -> f (g x)

(* For haskellers :) *)
let ( $ ) f x = f x

(* Apply partially while skipping one parameter *)
let ( *- ) f y = fun x -> f x y
