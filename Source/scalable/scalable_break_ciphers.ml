(** Factoring bitarrays into primes *)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
 *)
open Scalable;;
open Scalable_basic_arithmetics;;

let mod_power x n m =
  let rec calcul_puipui x n m =
    if (to_int n) = 0 then [0;1] else if (to_int n) = 1 then (mod_b x m) else
      if (mod_b n (from_int 2)) = [0] then calcul_puipui (mod_b (mult_b (mod_b x m) (mod_b x m)) m) (quot_b n (from_int 2)) m
      else mod_b (mult_b (mod_b x m) (calcul_puipui (mod_b x m) (diff_b n (from_int 1)) m)) m
  in calcul_puipui x n m;;

let prime_mod_power x n p =
  if (((mod_b x p) = [0]) || (compare_n p n) = 1) then mod_power x n p else
    mod_power x (mod_b n (diff_n p (from_int 1))) p;;

let is_prime n =
  if (to_int n) = 2 then true else
  let max = from_int(int_of_float(sqrt(float_of_int(to_int n)))+1) in
  let rec test_premier n max =
    if (to_int max) = 1 then true else
      if (mod_b n max) = (from_int 0) then false else
        test_premier n (diff_b max (from_int 1))
  in test_premier n max;;

let is_pseudo_prime p test_seq =
  if (to_int p) = 2 then true else if (is_prime p) then true else
  let rec tchoupi_a_epita p test_seq =
    match test_seq with
    |[] -> true
    |tete::reste -> if ((compare_n p tete) = 0) then true else
                      if (to_int (prime_mod_power tete (diff_n p (from_int 1)) p)) <> 1 then false else
                        tchoupi_a_epita p reste
  in tchoupi_a_epita p test_seq;;

let init_eratosthenes n =
  if (to_int n) < 2 then invalid_arg "Error [init_erathostenes_scalable] : n must be >= [0;0;1]" else
    if (to_int n) = 2 then [[0;0;1]] else
      let n = if (mod_b n [0;0;1]) = from_int 0 then (diff_b n [0;1]) else n in
      (*let depart = if (mod_b n (from_int 2)) = (from_int 0) then (diff_b n (from_int 1)) else n in*)
    let rec eratostos n liste x =
      if (to_int x) > 2 then (eratostos n (x::liste) (diff_b x [0;0;1])) else [0;0;1]::liste
    in let temp = eratostos n [[]] n in
       let rec enleve_last liste =
         match liste with
         |[] -> []
         |tete::reste -> if tete = [] then (enleve_last reste) else (tete::(enleve_last reste))
       in enleve_last temp;;

let eratosthenes n =
  if (to_int n) < 2 then invalid_arg "Error [eratosthenes_scalable] : n must be >= 2" else
    if (to_int n) = 3 then [(from_int 2);(from_int 3)] else
      let liste = init_eratosthenes n in
      let rec eratostos n liste x =
        match liste with
        |[] -> [[]] (* Not excepted to happen *)
        |tete::reste -> if is_pseudo_prime tete liste then
                          tete::(eratostos n reste x)
                        else eratostos n reste x
      in let temp = eratostos n liste (from_int 2)
         in let rec enleve_last liste =
              match liste with
              |[] -> []
              |tete::reste -> if tete = [] then (enleve_last reste) else (tete::(enleve_last reste))
            in enleve_last temp;;

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key = (* ([], []) *)
  let (truc, useless) = key in
  let objectif = (from_int (int_of_float(sqrt (float_of_int (to_int (truc)))))) in
  let tableau = eratosthenes (mult_b objectif [0;0;1]) in
  let rec casse n tab =
    match tab with
    |[] -> n
    |tete::reste -> if (compare_n tete n) = 1 then tete else (casse n reste)
  in let temp = (casse objectif tableau) in ((quot_b truc temp), temp);;
