(** Factoring Builtin int primes *)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
 *)
(*
#mod_use "power.ml";;
#mod_use "generate_primes.ml";;
#mod_use "test_primes.ml";;
 *)

open Builtin
open Basic_arithmetics

let is_prime n =
  let max = int_of_float(sqrt(float_of_int(n))) in
  let rec test_premier n max =
    if max = 1 then true else
      if modulo n max = 0 then false else
        test_premier n (max-1)
  in test_premier n max;;

let mod_power x n m =
  let rec puipui x n m =
    if n = 0 then 1 else if n = 1 then (modulo x m) else
      if (modulo n 2) = 0 then puipui (modulo ((modulo x m)*(modulo x m)) m) (n/2) m
      else modulo ((modulo x m) * puipui (modulo x m) (n-1) m) m
  in puipui x n m;;

let prime_mod_power x n p =
  if ((modulo x p = 0) || (n < p)) then mod_power x n p else
    mod_power x (modulo n (p-1)) p;;

let is_pseudo_prime p test_seq =
  let rec let_s_go p test_seq =
    match test_seq with
    |[] -> true
    |tete::reste -> if ((p = tete) && (is_prime p)) then true else
                      if (prime_mod_power tete (p-1) p) <> 1 then
                        false else let_s_go p reste
  in let_s_go p test_seq;;

let init_eratosthenes n =
  if n < 2 then invalid_arg "Error [init_eratosthenes] : n must be >= 2" else
  let depart = if modulo n 2 = 0 then (n-1) else n in
  let rec eratostos n liste x =
    if x >= 2 then eratostos n (x::liste) (x-2)
    else liste
  in 2::(eratostos n [] depart);;

let eratosthenes n =
  if n < 2 then invalid_arg "Error [eratosthenes] : n must be >= 2" else
    if n = 3 then [2; 3] else
    let liste = init_eratosthenes n in
    let rec eratostos n liste x =
        match liste with
        |[] -> []
        |tete::reste -> if is_pseudo_prime tete liste then
                          tete::(eratostos n reste x)
                        else eratostos n reste x
    in eratostos n liste 2;;

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (truc, useless) = key in
  let objectif = (int_of_float(sqrt (float_of_int truc))) in
  let tableau = eratosthenes (objectif*2) in
  let rec casse n tab =
    match tab with
    |[] -> n
    |tete::reste -> if tete > n then tete else casse n reste
  in let temp = casse objectif tableau in (truc/temp, temp);;

(*
break (99400891, 1);;
eratosthenes 9999;;
9967*9973;;
eratosthenes 99;;
 *)
