(** Test suites for z_power ml file using alcotest. *)

open Alcotest
open Z
open Z_power

let sprintf = Printf.sprintf
let zt = testable pp_print Z.equal

let z_mod_power_tests () =
  let cases =  [((of_int (-1), of_int 12, of_int 10), 1); ((of_int (-1), of_int 11, of_int 11), 10); ((of_int 0, of_int 2, of_int 3),   0);
                ((of_int 3, of_int 1, of_int 3),   0); ((of_int 5, of_int 0, of_int 2),     1); ((of_int (-2), of_int 2, of_int 5),  4);
                ((of_int (-2), of_int 3, of_int 9),   1); ((of_int 2, of_int 5, of_int 17),   15); ((of_int 3, of_int 3, of_int 17), 10)]
  and do_check ((x, n, m), expected) =
    check int (sprintf "%i^%i [%i]" (to_int x) (to_int n) (to_int m)) expected (to_int (mod_power (x) (n) (m)))
  in
  List.iter do_check cases

let z_prime_mod_power_tests () =
    let cases = [((of_int (-1), of_int 12, of_int 7), 1); ((of_int (-1), of_int 11, of_int 11), 10); ((of_int 0, of_int 2, of_int 3),   0);
                 ((of_int 3, of_int 1, of_int 3),   0); ((of_int 5, of_int 0, of_int 2),     1); ((of_int (-2), of_int 2, of_int 5),  4);
                 ((of_int (-2), of_int 3, of_int 5),  2); ((of_int 2, of_int 5, of_int 17),   15); ((of_int 3, of_int 3, of_int 17), 10)]
    and do_check ((x, n, p), expected) =
        check int (sprintf "%i^%i [%i]" (to_int x) (to_int n) (to_int p)) expected (to_int (prime_mod_power x n p))
    in
    List.iter do_check cases

(****************************************************************************)
(****************************************************************************)

let z_power_set =
  [("Mod Pow Z", `Quick, z_mod_power_tests);
   ("Prime Mod Pow Z", `Quick, z_prime_mod_power_tests)]
