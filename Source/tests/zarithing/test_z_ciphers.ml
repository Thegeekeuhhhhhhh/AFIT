(** Test suites for builtin cifers ml file using alcotest. *)

open Alcotest
open Z
open Test_tools
open Z_ciphers

let sprintf = Printf.sprintf
let zt = testable pp_print Z.equal

let z_generate_keys_rsa_tests () =
    let cases = [((9967, 9973), true)]
    and do_check ((p, q), expected) =
        let ((_, e), (n, d)) = generate_keys_rsa (of_int p) (of_int q)
        and phin = (pred (of_int p)) * (pred (of_int q))
        and is_inverse x y n = rem ((rem x n) * (rem y n)) n = one in
        check
            bool
            (sprintf "generate_rsa_keys: with %i and %i (we check if e is the inverse of d modulo n)" (p) (q))
            true
            (if rem e phin = one then false else is_inverse e d phin)
    in
    List.iter do_check cases

let z_encrypt_rsa_tests () =
    let cases = [((281237, (99400891, 36199003)), 70133953)]
    and do_check ((m, (n, e)), expected) =
        check int (sprintf "encrypt_rsa: %i with n=%i and e=%i" (m) (n) (e)) expected (to_int(encrypt_rsa (of_int m) ((of_int n), (of_int e))))
    in
    List.iter do_check cases

let z_decrypt_rsa_tests () =
    let cases = [((70133953, (99400891, 30869683)), 281237)]
    and do_check ((m, (n, e)), expected) =
        check int (sprintf "decrypt_rsa: %i with n=%i and e=%i" (m) (n) (e)) expected (to_int(decrypt_rsa (of_int m) ((of_int n), (of_int e))))
    in
    List.iter do_check cases

let z_el_gamal_tests () =
    let cases = [((of_int 100000007), (of_int 42))]
    and do_check (p, msg) =
        (* No need for an expected value, we encrypt and decrypt a message so
         * the expected result is the original message *)
        let (g, p) = public_data_g p in
        let (pub, priv) = generate_keys_g (g, p) in
        let (g_k, xA_k) = encrypt_g msg (g, p) pub in
        check
            int
            (sprintf "el_gamal functions: keys generated with p=%i, message %i is encrypted and then decrypted, we check if the result is equal to the original message"
                     (to_int p) (to_int msg))
            (to_int msg)
            (to_int(decrypt_g ((g_k), (xA_k)) priv (g, p)))
    in
    List.iter do_check cases

let z_ciphers_set =
  [("Z Generating RSA key function", `Quick, z_generate_keys_rsa_tests);
   ("Z Encrypt RSA function", `Quick, z_encrypt_rsa_tests);
   ("Z Decrypt RSA function", `Quick, z_decrypt_rsa_tests);
   ("Z El Gamal functions", `Quick, z_el_gamal_tests)
  ]
