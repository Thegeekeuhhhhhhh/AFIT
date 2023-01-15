(** Test suites for big int test_primes ml file using alcotest. *)

open Alcotest
open Z
open Test_tools
open Z_test_primes

let sprintf = Printf.sprintf
let zt = testable pp_print Z.equal

let z_is_prime_tests () =
    let cases = [(of_int 2, true  ); (of_int 3, true  ); (of_int 5, true);
                 (of_int 7, true  ); (of_int 11, true ); (of_int 13, true);
                 (of_int 4, false ); (of_int 6, false ); (of_int 12, false);
                 (of_int 45, false); (of_int 77, false); (of_int 63, false)]
    and do_check (p, expected) =
        check bool (sprintf "is_prime: %i" (to_int p)) expected (is_prime p)
    in
    List.iter do_check cases

(****************************************************************************)
(****************************************************************************)

let z_test_primes_set =
  [("Z Is Prime Function", `Quick, z_is_prime_tests)]
