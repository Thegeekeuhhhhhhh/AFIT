(** Testing for primality *)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
#mod_use "scalable_power.ml";;
 *)
open Scalable;;
open Scalable_basic_arithmetics;;
open Scalable_power;;
   (*
open Scalable_power
    *)

(** Deterministic primality test *)
let is_prime n =
  if (compare_b n [0;0;1]) = 0 then true else
  let max = from_int(int_of_float(sqrt(float_of_int(to_int n)))+1) in
  let rec test_premier n max =
    if (compare_b max [0;1]) = 0 then true else
      if (mod_b n max) = [] then false else
        test_premier n (diff_b max [0;1])
  in test_premier n max;;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq =
  if (to_int p) < 2 then invalid_arg "Error [is_pseudo_prime_scalable] : p must be > [0;0;1]" else
    if (to_int p) = 2 then true else if (is_prime p) then true else
      if (mod_b p [0;0;1]) = (from_int 0) then false else
        let rec verif_secure p test_seq liste_secure =
          match liste_secure with
          |[] -> false
          |tete::reste -> if tete = p then true else
                            verif_secure p test_seq reste in
        let rec tchoupi_a_epita p test_seq =
          match test_seq with
          |[] -> true
          |tete::reste -> if p = tete then true else
                            if (prime_mod_power tete (diff_n p [0;1]) p) <> [0;1]
                            then if p = tete then true
                                 else verif_secure p test_seq []
            else tchoupi_a_epita p reste
        in tchoupi_a_epita p test_seq;;


  (*
  if (to_int p) < 2 then invalid_arg "Error [is_pseudo_prime_scalable] : p must be > [0;0;1]" else
    if (to_int p) = 2 then true else if (is_prime p) then true else
        if (mod_b p [0;0;1]) = (from_int 0) then false else
  let rec tchoupi_a_epita p test_seq =
    match test_seq with
    |[] -> true
    |tete::reste -> if (compare_b p tete) = 0 then true else
                      if (prime_mod_power tete (diff_n p [0;1]) p) <> [0;1] then false
                      else tchoupi_a_epita p reste
  in tchoupi_a_epita p test_seq;;*)
