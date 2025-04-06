(** Factoring bitarrays into primes *)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
 *)
open Scalable;;
open Scalable_basic_arithmetics;;

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (key, useless) = key in
  let racine = from_int(int_of_float(sqrt(float_of_int(to_int(key))))) in
  let new_racine = if mod_b racine [0;0;1] = [] then add_b racine [0;1] else racine in
  let rec cherche_cle key cle =
    match key with
    |[] -> [1;1]
    |[0;1] -> [1;1]
    |_ -> if mod_b key cle = [] then cle else cherche_cle key (diff_b cle [0;0;1])
  in let a = cherche_cle key new_racine in (a, (quot_b key a));;

(*  ([], [])
  let (truc, useless) = key in
  let objectif = (from_int (int_of_float(sqrt (float_of_int (to_int (truc)))))) in
  let tableau = eratosthenes (mult_b objectif [0;0;1]) in
  let rec casse n tab =
    match tab with
    |[] -> n
    |tete::reste -> if (compare_n tete n) = 1 then tete else (casse n reste)
  in let temp = (casse objectif tableau) in ((quot_b truc temp), temp);;  *)
