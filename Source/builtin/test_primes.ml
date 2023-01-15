(** Testing for primality *)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;
*)
open Builtin;;
open Basic_arithmetics;;
open Power;;

(** Deterministic primality test *)
let is_prime n =
  if n = 2 then true else
  let max = (int_of_float(sqrt(float_of_int(n)))+1) in
  let rec test_premier n max =
    if max = 1 then true else
      if modulo n max = 0 then false else
        test_premier n (max-1)
  in test_premier n max;;

(*
is_prime 2;;
is_prime 3;;
is_prime 5;;
is_prime 7;;
is_prime 9;;
is_prime 11;;
is_prime 15;;
 *)

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
  let rec verif_secure p test_seq liste_secure =
    match liste_secure with
    |[] -> false
    |tete::reste -> if tete = p then true else
                      verif_secure p test_seq reste in
  let rec let_s_go p test_seq =
    match test_seq with
    |[] -> true
    |tete::reste -> if (prime_mod_power tete (p-1) p) <> 1 then if p = tete then true else
                                                                  verif_secure p test_seq [341; 561; 645; 1105; 1387; 1729; 1905; 2047; 2465; 2701; 2821; 3277; 4033; 4369; 4371;4681;5461;6601;7957;8321;8481;8911;10261;10585;11305;12801;13741;13747;13981;14491;15709;15841;16705;18705;18721;19951;23001;23377;25761;29341;41041;46657;52633;62745]
      else let_s_go p reste
  in let_s_go p test_seq;;

(*
is_pseudo_prime 1105 [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73; 79; 83; 89; 97; 101; 103; 107; 109; 113; 127];;
is_pseudo_prime 341 [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73; 79; 83; 89; 97; 101; 103; 107; 109; 113];;

prime_mod_power 2 10 11;;
mod_power 2 10 11;;
prime_mod_power 4 10 11;;
prime_mod_power 5 10 11;;
prime_mod_power 20 10 11;;
is_pseudo_prime 2 [2;4;8;12];;
is_pseudo_prime 11 [2;4;5;20];;
is_pseudo_prime 23 [2;9;15;18];;
is_pseudo_prime 27 [30;41;52];;
is_pseudo_prime 29 [30;41;52];;
 *)

