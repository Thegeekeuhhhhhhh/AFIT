(** Factoring big integers into primes *)

open Z

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (key, useless) = key in
  let racine = sqrt key in
  let new_racine = if rem racine (succ one) = zero then (succ racine) else racine in
  let rec cherche_cle key cle =
    match (to_int key) with
    |0 -> (minus_one)
    |1 -> (minus_one)
    |_ -> if rem key cle = zero then cle else cherche_cle key (pred (pred cle))
  in let a = cherche_cle key new_racine in (a, div key a);;
