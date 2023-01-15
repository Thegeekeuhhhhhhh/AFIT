(** Test suites for big int encoding_msg ml file using aclotest. *)

open Alcotest
open Z
open Z_encoding_msg

let sprintf = Printf.sprintf
let zt = testable pp_print Z.equal

let z_encode_tests () =
    let cases = [(("Bashar", 7), 2294023860466)]
    and do_check ((str, bits), expected) =
        check int (sprintf "encode: \"%s\" on %i bits" str bits) expected (to_int(encode str bits))
    in
    List.iter do_check cases

let z_decode_tests () =
    let cases = [((2294023860466, 7), "Bashar")]
    and do_check ((msg, bits), expected) =
        check string (sprintf "decode: %i on %i bits" msg bits) expected (decode (of_int msg) bits)
    in
    List.iter do_check cases

(****************************************************************************)
(****************************************************************************)

let z_encoding_msg_set =
    [("Encode function", `Quick, z_encode_tests);
     ("Decode function", `Quick, z_decode_tests)]
