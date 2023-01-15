(** Test suites for builtin basic_arithmetic ml file using alcotest. *)

open Alcotest
open Test_tools
open Scalable

let sprintf = Printf.sprintf

let from_int_tests () =
    let cases =
        [(10, [0;0;1;0;1]); ((-10), [1;0;1;0;1]); (0, []);]
    and do_check (p, expected) =
        check
            (list int)
            (sprintf "from_int: %d" p)
            expected
            (from_int p)
    in
    List.iter do_check cases

let to_int_tests () =
    let cases =
        [([0;0;1;0;1], 10); ([1;0;1;0;1], (-10)); ([], 0);]
    and do_check (p, expected) =
        check
            int
            (sprintf "to_int: %d" expected)
            expected
            (to_int p)
    in
    List.iter do_check cases

let compare_b_tests () =
    let cases =
        [
            (from_int 54, from_int 22, 1);
            (from_int (-10), from_int 11, (-1));
            (from_int 10, from_int (-11), 1);
            (from_int (-10), from_int (-5), (-1));
            (from_int 4, from_int 4, 0);
            (from_int (-5), from_int (-5), 0);
            (from_int 0, from_int 14, (-1));
            (from_int 20, from_int 0, 1);
            (from_int 0, from_int 0, 0);
        ]
    and do_check (a, b, expected) =
        check
            int
            (sprintf "compare_b: %d" expected)
            expected
            (compare_b a b)
    in
    List.iter do_check cases

let add_b_tests () =
    let cases =
        [
            ((from_int 4), (from_int 3), 7);
            ((from_int 5), (from_int (-3)), 2);
            ((from_int (-4)), (from_int 3), (-1));
            ((from_int (-5)), (from_int (-2)), (-7));
            ((from_int 2), (from_int 4), 6);
            ((from_int 4), (from_int (-6)), (-2));
            ((from_int (-3)), (from_int 5), 2);
            ((from_int (-3)), (from_int (-5)), (-8));
            ((from_int 0), (from_int (-5)), (-5));
            ((from_int 0), (from_int 2), 2);
            ((from_int (-2)), (from_int 0), (-2));
            ((from_int 4), (from_int 0), 4);
            ((from_int 4), (from_int 6), 10);
            ((from_int (-6)), (from_int (-6)), (-12));
        ]
    and do_check (a, b, expected) =
        check
            int
            (sprintf "add_b: %d + %d = %d" (to_int a) (to_int b) expected)
            expected
            (to_int (add_b a b))
    in
    List.iter do_check cases

let diff_b_tests () =
    let cases =
        [
            ((from_int 5), (from_int 3), 2);
            ((from_int 6), (from_int (-2)), 8);
            ((from_int (-6)), (from_int 2), (-8));
            ((from_int (-6)), (from_int (-2)), (-4));
            ((from_int 3), (from_int 5), (-2));
            ((from_int 3), (from_int (-6)), 9);
            ((from_int (-3)), (from_int 6), (-9));
            ((from_int (-3)), (from_int (-6)), 3);
            ((from_int 0), (from_int (-5)), 5);
            ((from_int 0), (from_int 5), (-5));
            ((from_int (-6)), (from_int 0), (-6));
            ((from_int 5), (from_int 0), 5);
            ((from_int 5), (from_int 5), 0);
            ((from_int (-5)), (from_int (-5)), 0);
        ]
    and do_check (a, b, expected) =
        check
            int
            (sprintf "diff_b: %d - %d = %d" (to_int a) (to_int b) expected)
            expected
            (to_int (diff_b a b))
    in
    List.iter do_check cases

let mult_b_tests () =
    let cases =
        [
            ((from_int 6), (from_int 2), 12);
            ((from_int 6), (from_int (-2)), (-12));
            ((from_int (-6)), (from_int 2), (-12));
            ((from_int (-6)), (from_int (-2)), 12);
            ((from_int 3), (from_int 5), 15);
            ((from_int 3), (from_int (-5)), (-15));
            ((from_int (-3)), (from_int 5), (-15));
            ((from_int (-3)), (from_int (-5)), 15);
            ((from_int 0), (from_int (-5)), 0);
            ((from_int 0), (from_int 5), 0);
            ((from_int (-5)), (from_int 0), 0);
            ((from_int 5), (from_int 0), 0);
            ((from_int 5), (from_int 5), 25);
            ((from_int (-5)), (from_int (-5)), 25);
        ]
    and do_check (a, b, expected) =
        check
            int
            (sprintf "mult_b: %d * %d = %d" (to_int a) (to_int b) expected)
            expected
            (to_int (mult_b a b))
    in
    List.iter do_check cases

let quot_b_tests () =
    let cases =
        [
            (10, 3), 3;
            (-10, 3), -4;
            (8, 2), 4;
            (-1, 11), -1;
            (-10, 2), -5;
        ]
    and do_check ((a, b), expected) =
        check
            int
            (sprintf "quot_b: %d / %d = %d" a b expected)
            expected
            (to_int (quot_b (from_int a) (from_int b)))
    in
    List.iter do_check cases

let mod_b_tests () =
    let cases =
        [
            (10, 3), 1;
            (10, 2), 0;
            (10, 2), 0;
            (-10, 2), 0;
            (-1, 11), 10
        ]
    and do_check ((a, b), expected) =
        check
            int
            (sprintf "mod_b: %d [%d] = %d" a b expected)
            expected
            (to_int (mod_b (from_int a) (from_int b)))
    in
    List.iter do_check cases

(*
let scalable_set =
    [
      ("From Int O_o", `Quick, from_int_tests);
      ("To Int o_O", `Quick, to_int_tests);
      ("Compare O_O", `Quick, compare_b_tests);
      ("Add B +_+", `Quick, add_b_tests);
      ("Diff B -_-", `Quick, diff_b_tests);
      ("Mult B *_*", `Quick, mult_b_tests);
      ("Quot B /_/", `Quick, quot_b_tests);
      ("Mod B %_%", `Quick, mod_b_tests);
    ]
 *)


(****************************************************************************)
(****************************************************************************)


let e_from_int_tests () =
    let cases =
        [(32, [0; 0; 0; 0; 0; 0; 1]); (-3, [1; 1; 1]); (7, [0; 1; 1; 1])]
    and do_check (intgr , expected) =
        check
            (list int)
            (sprintf "from_int: %i" intgr)
            expected
            (from_int intgr)
    in
    List.iter do_check cases

let e_to_int_tests () =
    let cases =
        [([0; 0; 0; 0; 0; 0; 1], 32); ([1; 1; 1], -3); ([0; 1; 1; 1], 7)]
    and do_check (bitarray , expected) =
        check
            (int)
            (sprintf "to_int: %s" (string_of_intlist bitarray))
            expected
            (to_int bitarray)
    in
    List.iter do_check cases

let e_bigger_tests () =
    let cases =
        [([0;0;1;0;0;1], [0;0;1;1;0;1]), false;
         ([0;0;0;1;0;1], [0;1;0;0;0;1]), true;
         ([0;1;0;0;0;1], [0;1;0;0;0;1]), false;
         ([1;0;0;1;0;1], [1;1;1;0;0;1]), false;
         ([1;0;1;0;0;1],[1;1;1;0;0;1]), true]
    and do_check ((a, b), expected) =
        check
            (bool)
            (sprintf "bigger_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (a >> b)
    in
    List.iter do_check cases

let e_smaller_tests () =
  let cases =
    [([0;0;1;0;0;1], [0;0;1;1;0;1]), true;
     ([0;0;0;1;0;1], [0;1;0;0;0;1]), false;
     ([0;1;0;0;0;1], [0;1;0;0;0;1]), false;
     ([1;0;0;1;0;1], [1;1;1;0;0;1]), true;
     ([1;0;1;0;0;1],[1;1;1;0;0;1]), false]
    and do_check ((a, b), expected) =
        check
            (bool)
            (sprintf "smaller_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (a << b)
    in
    List.iter do_check cases


let e_biggerEqual_tests () =
  let cases =
    [([0;0;1;0;0;1], [0;0;1;1;0;1]), false;
     ([0;0;0;1;0;1], [0;1;0;0;0;1]), true;
     ([0;1;0;0;0;1], [0;1;0;0;0;1]), true;
     ([1;0;0;1;0;1], [1;1;1;0;0;1]), false;
     ([1;0;1;0;0;1],[1;1;1;0;0;1]), true]
    and do_check ((a, b), expected) =
        check
            (bool)
            (sprintf "biggerEqual_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (a >>= b)
    in
    List.iter do_check cases

let e_smallerEqual_tests () =
    let cases =
      [([0;0;1;0;0;1], [0;0;1;1;0;1]), true;
       ([0;0;0;1;0;1], [0;1;0;0;0;1]), false;
       ([0;1;0;0;0;1], [0;1;0;0;0;1]), true;
       ([1;0;0;1;0;1], [1;1;1;0;0;1]), true;
       ([1;0;1;0;0;1],[1;1;1;0;0;1]), false]
    and do_check ((a, b), expected) =
        check
            (bool)
            (sprintf "smallerEqual_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (a <<= b)
    in
    List.iter do_check cases

let e_add_b_tests () =
  let cases =
    [([0;0;1;0;0;1], [0;0;1;1;0;1]), [0;0;0;0;1;0;1];
     ([0;0;0;1;0;1], [0;1;0;0;0;1]), [0;1;0;1;0;0;1];
     ([0;1;0;0;0;1], []), [0;1;0;0;0;1];
     ([1;0;0;1;0;1], [0;0;0;1;0;1]), []]
    and do_check ((a, b), expected) =
        check
            (list int)
            (sprintf "add_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (add_b a b)
    in
    List.iter do_check cases

let e_diff_b_tests () =
  let cases =
    [([0;0;1;0;0;1], [0;0;1;1;0;1]), [1;0;0;1];
     ([0;0;0;1;0;1], [0;1;0;0;0;1]), [0;1;1];
     ([0;1;0;0;0;1], []), [0;1;0;0;0;1];
     ([1;0;0;1;0;1], [1;0;0;1;0;1]), []]
    and do_check ((a, b), expected) =
        check
            (list int)
            (sprintf "diff_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (diff_b a b)
    in
    List.iter do_check cases

let e_mult_b_tests () =
  let cases =
    [([0;1], [0;0;1;1;0;1]), [0;0;1;1;0;1];
     ([1;1], [0;0;1;1;0;1]), [1;0;1;1;0;1];
     ([], [0;0;1;1;0;1]), [];
     ([0;0;1], [0;1;0;0;0;1]), [0;0;1;0;0;0;1];
     ([0;1;1], [0;1;1]), [0;1;0;0;1]]
    and do_check ((a, b), expected) =
        check
            (list int)
            (sprintf "mult_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (mult_b a b)
    in
    List.iter do_check cases

let e_mod_b_tests () =
  let cases =
    [([0;1], [0;0;1;1;0;1]), [0;1];
     ([0;1;0;1;0;1], [0; 0; 1]), [0;1];
     ([0;0;1;1;0;1], [0; 0; 1]), [];
     ([0;1;1;0;0;1], [0; 0; 0; 1]), [0; 1; 1]]
  and do_check ((a, b), expected) =
        check
            (list int)
            (sprintf "mod_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (mod_b a b)
    in
    List.iter do_check cases

(****************************************************************************)
(****************************************************************************)

let all_scalables_set =
  [("From Int O_o", `Quick, from_int_tests);
   ("To Int o_O", `Quick, to_int_tests);
   ("Compare O_O", `Quick, compare_b_tests);
   ("Add B +_+", `Quick, add_b_tests);
   ("Diff B -_-", `Quick, diff_b_tests);
   ("Mult B *_*", `Quick, mult_b_tests);
   ("Quot B /_/", `Quick, quot_b_tests);
   ("Mod B %_%", `Quick, mod_b_tests);
   ("Casting to bitarray function", `Quick, e_from_int_tests);
   ("Casting to int function", `Quick, e_to_int_tests);
   ("Bigger bitarray function", `Quick, e_bigger_tests);
   ("Smaller bitarray function", `Quick, e_smaller_tests);
   ("Bigger or Equal bitarray function", `Quick, e_biggerEqual_tests);
   ("Bigger or Equal bitarray function", `Quick, e_smallerEqual_tests);
   ("add_b bitarray function", `Quick, e_add_b_tests);
   ("diff_b or Equal bitarray function", `Quick, e_diff_b_tests);
   ("mult_b or Equal bitarray function", `Quick, e_mult_b_tests);
   ("mod_b or Equal bitarray function", `Quick, e_mod_b_tests);
  ]
