(** Power function implementations for builtin integers *)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
 *)

open Builtin;;
open Basic_arithmetics;;

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  if n < 1 then 1 else
    let n = n*sign n in
    let rec calcul_puipui x n result =
      if n = 1 then result else
        calcul_puipui x (n-1) (result*x)
    in calcul_puipui x n x;;

(*
pow 5 2;;
pow 5 (-10);;
pow 4 0;;
pow 5 3;;
 *)

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)

let power x n =
  if n < 1 then 1 else
    let rec cherche_puipui x n =
      if n < 1 then 1 else
        if (modulo n 2) = 0 then cherche_puipui (x*x) (n/2)
        else (cherche_puipui (x*x) (n/2))*x
    in cherche_puipui x n;;

(*
power 5 0;;
power 96713 87567333;;
power 2 21;;
power 1 5;;
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  let rec puipui x n m =
    if n = 0 then 1 else if n = 1 then (modulo x m) else
      if (modulo n 2) = 0 then puipui (modulo ((modulo x m)*(modulo x m)) m) (n/2) m
      else modulo ((modulo x m) * puipui (modulo x m) (n-1) m) m
  in puipui x n m;;

(*
mod_power (-1) 12 10;; (* 1 *)
mod_power(-1) 11 11;; (* 10 *)
mod_power 0 2 3;; (* 0 *)
mod_power 3 1 3 (* 0 *);;
mod_power 5 0 2;; (* 1 *)
mod_power (-2) 2 5 (* 4 *);;
mod_power (-2) 3 9 (* 1 *);;
mod_power 2 5 17 (* 15 *);;
mod_power 3 3 17 (* 10 *);;
mod_power 2 10 11;;
mod_power 9 12 10;;
modulo (-1) 10;;
mod_power (-1) 1 11;;
mod_power (-1) 12 10;;
mod_power 5 2 11;;
mod_power 7 2 10;;
mod_power 10 2 4;;
mod_power 4 2 3;;
mod_power 5 3 4;;
mod_power 15060468 16541104 1242451;;
mod_power 281237 36199003 99600891;;
mod_power 281237 99400891 36199003;;
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  if ((modulo x p = 0) || (n < p)) then mod_power x n p else
    mod_power x (modulo n (p-1)) p;;

(*
prime_mod_power (-10) 11 11;;
prime_mod_power (-1) 12 7;;
prime_mod_power 3 20 7;;
prime_mod_power 2817 99466492 36;;
prime_mod_power 2 2 3;;
prime_mod_power 2 16 3;;
prime_mod_power 2 15 3;;
 *)
