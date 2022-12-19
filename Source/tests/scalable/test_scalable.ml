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

let default_set =
    [
      ("From Int", `Quick, from_int_tests);
      ("To Int", `Quick, to_int_tests);
      ("Compare", `Quick, compare_b_tests);
      ("Add B", `Quick, add_b_tests);
      ("Diff B", `Quick, diff_b_tests);
      ("Mult B", `Quick, mult_b_tests);
      ("Quot B", `Quick, quot_b_tests);
      ("Mod B", `Quick, mod_b_tests);
    ]
