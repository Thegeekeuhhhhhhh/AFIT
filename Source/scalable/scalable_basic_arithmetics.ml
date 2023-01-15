(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)

(*
#mod_use "scalable.ml";;
 *)
open Scalable;;

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b bA bB =
  let rec calcul bA bB =
    if bB = [] then bA else (calcul bB (mod_b bA bB))
  in calcul bA bB;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
  let rec theoreme_mais_pas_de_pythagore bA bB =
    if bB = [] then (bA, [0; 1], []) else
      let (diviseur, nb1, nb2) = theoreme_mais_pas_de_pythagore bB (mod_b bA bB) in
      let (q, r) = div_b bA bB in (diviseur, nb2, (diff_b nb1 (mult_b q nb2)))
  in let (a, b, c) = theoreme_mais_pas_de_pythagore bA bB in (b, c, a);;
