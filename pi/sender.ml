let rec expmod a b m =
  if b = 0 then 1
  else ( (if b mod 2 = 0 then 1 else a ) * expmod (a*a) (b/2) m ) mod m

let nth_digit n =
  let s j =
    let s = ref 0.0 in
    for k = 0 to n do
      let r = 8*k+j in
      s := !s +. fst (modf ((float (expmod 16 (n - k) r)) /. (float r) ))
    done;

    let t = ref (0.01) in
    let k = ref (n + 1) in
    let newt = ref (0.0) in
    
    while !t <> !newt do
      t := !newt;
      newt := !t +. (16.0 ** float (n - !k)) /. float (8* !k + j);
      incr k
    done;
    !s +. !t
  in
  truncate (16.0 *. (4.0 *. (s 1) -. 2.0 *. (s 4) -. (s 5) -. (s 6)))

module Cl = Socket.Client.Client(Cfg)
open Cl

let k = int_of_string Sys.argv.(1)

let (_, out_ch) = open_channel k

let rec loop n =
  bind (put ) (fun () -> print_endline "foo"; loop (3+n))

let _ = run (loop k)

