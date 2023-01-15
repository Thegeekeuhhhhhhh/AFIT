(** Basic arithmetics with builtin integers *)

(*
#mod_use "builtin.ml";;
*)
open Builtin;;

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
 *)
let rec gcd a b =
  if b = 0 then a else (gcd b (modulo a b));;

(*
gcd 32 6;;
gcd 18 12;;
gcd (-18) (-12);;
gcd 2 20;;
gcd 3 20;;
gcd 21 20;;
gcd 19 20;;
gcd 20 19;;
gcd 5 1;; (* 1 *)
gcd 5 5;; (* 5 *)
gcd 1 1;; (* 1 *)
gcd 14 2;; (* 2 *)
gcd 14 4;; (* 2 *)
gcd 14 7;; (* 7 *)
gcd 14 10;; (* 2 *)
*)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
 *)

let bezout a b =
  let rec theoreme_mais_pas_de_pythagore a b =
    if b = 0 then (a, 1, 0) else
      let (diviseur, nb1, nb2) = theoreme_mais_pas_de_pythagore b (modulo a b) in
      let (q, r) = div a b in (diviseur, nb2, (nb1 - (q*nb2)))
  in let (a, b, c) = theoreme_mais_pas_de_pythagore a b in (b, c, a);;

(*
bezout 18 22;;
bezout 22 18;;
bezout 17 21;;
bezout 21 17;;
bezout 20 3;;
bezout 3 20;;
 *)
