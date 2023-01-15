open Alcotest
open Test_tools
open Test_builtin
open Test_builtin_basic_arithmetics
open Test_builtin_power
open Test_builtin_test_primes
open Test_builtin_generate_primes
open Test_builtin_encoding_msg
open Test_builtin_ciphers
open Test_builtin_break_ciphers

let builtin_test_suite  = [
    ("Builtin",               builtin_set);
    ("Arithmétique basique",  basic_arithmetics_set);
    ("Powers",                power_set);
    ("Test primes",           test_primes_set);
    ("Generate primes",       generate_primes_set);
    ("Encode / decode msgs",  encoding_msg_set);
    ("Ciphers",               ciphers_set);
    ("Break ciphers",         break_ciphers_set)
  ]

let () = run_to_xml "trace_builtin_1.xml" [builtin_test_suite]
