(** Testing for primality *)

open Z;;
open Z_power;;

(** Deterministic primality test
    @param n a big integer bigger or equal to 2.
 *)
let is_prime n =
  if (pred n) = one then true else
    let max = sqrt n in
    let rec test_premier n max =
      if max = one then true else
        if n mod max = zero then false else
          test_premier n (pred max)
    in test_premier n max;;
