(** Testing for primality *)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;
*)
open Builtin;;
open Basic_arithmetics;;
open Power;;

(*
let sign x =
  if x >= 0 then 1 else (-1);;

let modulo a b =
  if b = 0 then invalid_arg "Error [Modulo] : b can not be null" else
    let b = b * sign b in
    if a = 0 then 0 else
      if a < 0 then let rec modneg a b =
                      if a >= 0 then a else modneg (a+b) b
                    in modneg a b
      else let rec modpos a b =
             if a < 0 then (a+b) else modpos (a-b) b
           in modpos a b;;

let mod_power x n m =
  let rec puipui x n m =
    if n = 0 then 1 else
      modulo ((modulo x m)* puipui x (n-1) m) m
  in puipui x n m;;

let prime_mod_power x n p =
  if ((modulo x p = 0) || (n < p)) then mod_power x n p else
    let temp = modulo n (p-1) in
    mod_power x temp p;;
 *)

(** Deterministic primality test *)
let is_prime n =
  if n = 2 then true else
  let max = (int_of_float(sqrt(float_of_int(n)))+1) in
  let rec test_premier n max =
    if max = 1 then true else
      if modulo n max = 0 then false else
        test_premier n (max-1)
  in test_premier n max;;

(*
is_prime 2;;
is_prime 3;;
is_prime 5;;
is_prime 7;;
is_prime 9;;
is_prime 11;;
is_prime 15;;
 *)

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
  let rec let_s_go p test_seq =
    match test_seq with
    |[] -> true
    |tete::reste -> if ((p = tete) && (is_prime p)) then true else
                      if (prime_mod_power tete (p-1) p) <> 1 then
                        false else let_s_go p reste
  in let_s_go p test_seq;;

(*
prime_mod_power 2 10 11;;
mod_power 2 10 11;;
prime_mod_power 4 10 11;;
prime_mod_power 5 10 11;;
prime_mod_power 20 10 11;;
is_pseudo_prime 2 [2;4;8;12];;
is_pseudo_prime 11 [2;4;5;20];;
is_pseudo_prime 23 [2;9;15;18];;
is_pseudo_prime 27 [30;41;52];;
is_pseudo_prime 29 [30;41;52];;
 *)
