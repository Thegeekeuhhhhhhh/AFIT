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

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (key, useless) = key in
  let racine = int_of_float(sqrt(float_of_int(key))) in
  let new_racine = if racine mod 2 = 0 then racine+1 else racine in
  let rec cherche_cle key cle =
    match key with
    |0 -> (-1)
    |1 -> (-1)
    |_ -> if key mod cle = 0 then cle else cherche_cle key (cle-2)
  in let a = cherche_cle key new_racine in (a, key/a);;

(*
let break key =
  let (truc, useless) = key in
  let objectif = (int_of_float(sqrt (float_of_int truc))) in
  let tableau = eratosthenes (objectif*2) in
  let rec casse n tab =
    match tab with
    |[] -> n
    |tete::reste -> if tete > n then tete else casse n reste
  in let temp = casse objectif tableau in (truc/temp, temp);;

break (92834939, 9163921);;*)

(*
other_break 92834939;;
9634*9636;;
92834939 mod 9636;;
100711415*100711416;;
10142789312725007 mod 100711409;;
int_of_float(sqrt(float_of_int(10142789312725007)));;
10142789312725007 mod 100711415;;
10142789312725007 mod 100711413;;
10142789312725007 mod 100711411;;
10142789312725007 mod 100711409;;
other_break 10142789312725007;;
100711423*100711409;;
break (99400891, 1);;
eratosthenes 9999;;
9967*9973;;
eratosthenes 99;;
 *)
