(** Test suites for big int generate primes ml file using alcotest. *)

open Alcotest
open Z
module P = Z_test_primes
open Z_generate_primes

let sprintf = Printf.sprintf
let zt = testable pp_print Z.equal

let to_int_list liste =
  let rec a list =
    match list with
    |[] -> []
    |tete::reste -> (to_int tete)::a reste
  in a liste;;

let to_int_int_list liste =
  let rec a list =
    match list with
    |[] -> []
    |tete::reste -> let (x, y) = tete in ((to_int x), (to_int y))::a reste
  in a liste;;

let z_init_eratosthenes_tests () =
    let cases = [(2, [2]); (3, [2; 3]); (6, [2; 3; 5])]
    and do_check (n, expected) =
        check (list int) (sprintf "init_eratosthenes: %i" n) expected (to_int_list (init_eratosthenes (of_int n)))
    in
    List.iter do_check cases

let z_eratosthenes_tests () =
    let cases =  [(2, [2]); (3, [2; 3]); (6, [2; 3; 5]);
                  (25, [2; 3; 5; 7; 11; 13; 17; 19; 23])]
    and do_check (n, expected) =
        check (list int) (sprintf "eratosthenes: %i" n) expected (to_int_list(eratosthenes (of_int n)))
    in
    List.iter do_check cases

let z_double_primes_tests () =
    let cases = [((20, P.is_prime), [(2, 5); (3, 7); (5, 11); (11, 23)])]
    and do_check ((n, isprime), expected) =
        check (list (pair int int)) (sprintf "double_primes: %i" n) expected (to_int_int_list(double_primes (of_int n) isprime))
    in
    List.iter do_check cases

(****************************************************************************)
(****************************************************************************)

let z_generate_primes_set =
  [("Z Eratosthenes initialisation function", `Quick, z_init_eratosthenes_tests);
   ("Z Eratosthenes function", `Quick, z_eratosthenes_tests);
   ("Z Double primes function", `Quick, z_double_primes_tests)
  ]
