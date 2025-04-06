(** Power function implementations for bitarrays *)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
 *)
open Scalable;;
open Scalable_basic_arithmetics;;

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  if (compare_b n [0;1]) = (-1) then [0;1] else
    let rec calcul_puipui x n result =
      if (compare_b n [0;0;1]) = (-1) then result else
        calcul_puipui x (diff_b n [0;1]) (mult_b result x)
    in calcul_puipui x n x;;

(*
from_int 5421;;
from_int 8;;
diff_b [0;1;0;1;0;1] [1];;
mult_b [0; 1; 0; 1; 1; 0; 1; 0; 0; 1; 0; 1; 0; 1] [0; 1; 0; 1; 1; 0; 1; 0; 0; 1; 0; 1; 0; 1];;
pow [0; 1; 0; 1; 1; 0; 1; 0; 0; 1; 0; 1; 0; 1] [0;0;0;0;1];;*)

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n =
  if (compare_b n []) = 0 then [0;1] else
    let rec calcul_puipui x n =
      if (compare_b n []) = 0 then [1] else
        if (mod_b n [0;0;1]) = [] then calcul_puipui (mult_b x x) (quot_b n [0;0;1])
        else (mult_b (calcul_puipui (mult_b x x) (quot_b n [0;0;1])) x)
    in calcul_puipui x n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m =
  let rec calcul_puipui x n m =
    if n = [] then [0;1] else if n = [0;1] then (mod_b x m) else
      if (mod_b n [0;0;1]) = [] then calcul_puipui (mod_b (mult_b (mod_b x m) (mod_b x m)) m) (quot_b n [0;0;1]) m
      else mod_b (mult_b (mod_b x m) (calcul_puipui (mod_b x m) (diff_b n [0;1]) m)) m
  in calcul_puipui x n m;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
  if (((mod_b x p) = []) || (compare_n p n) = 1) then mod_power x n p else
    mod_power x (mod_b n (diff_n p [0;1])) p;;
