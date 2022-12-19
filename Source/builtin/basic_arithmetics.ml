(** Basic arithmetics with builtin integers *)

(*
#mod_use "builtin.ml";;
*)
open Builtin;;
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

let quot a b =
  if b = 0 then invalid_arg "Error [quot] : b can not be null" else
    if a = 0 then 0 else
      let rec calcul_quotient a b q =
        if (a-b) >= 0 then calcul_quotient (a-b) b (q+1) else q
      in let resultat = if a > 0 then (calcul_quotient (a*sign a) (b*sign b) 0)*(sign b) else
                          ((calcul_quotient (a*sign a) (b*sign b) 0)+1)*(sign b)*(-1) in
         if (resultat+1)*b = a then resultat+1 else resultat;;

let div a b =
  let q = (quot a b) and r = (modulo a b) in
  (q, r);;*)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
 *)
let rec gcd a b =
  if b = 0 then a else (gcd b (modulo a b));;
(*
  let (a, b) = ((a * sign a), (b * sign b)) in
  let (a, b) = if a < b then (b, a) else (a, b) in
  let pgcd = modulo a b in
  match pgcd with
  |0 -> b
  |y -> gcd b y;;
 *)

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
