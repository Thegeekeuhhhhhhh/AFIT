(** Test suites for big int break_cifers ml file using alcotest. *)

open Alcotest
open Z
open Z_break_ciphers

let sprintf = Printf.sprintf
let zt = testable pp_print Z.equal

let double_to_int k =
  let (a, b) = k in ((to_int a), (to_int b));;

let z_break_tests () =
    let cases = [((99400891, 36199003), (9967, 9973))]
    and do_check ((n, e), expected) =
        check
            (pair int int)
            (sprintf "break: n=%i and e=%i" n e)
            expected
            (double_to_int(break (((of_int n), (of_int e)))))
    in
    List.iter do_check cases

(****************************************************************************)
(****************************************************************************)

let z_break_ciphers_set =
    [("Z RSA key BREAKING BAD ><", `Quick, z_break_tests)]
