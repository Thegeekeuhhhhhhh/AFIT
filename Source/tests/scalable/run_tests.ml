open Alcotest
open Test_tools
open Test_scalable
open Test_scalable_basic_arithmetics
open Test_scalable_power
open Test_scalable_test_primes
open Test_scalable_generate_primes
open Test_scalable_encoding_msg
open Test_scalable_ciphers
open Test_scalable_break_ciphers

let scalable_test_suite =
  [("Scalable base",           all_scalables_set);
   ("S Arithmétique basique",  basic_arithmetics_set);
   ("S Power",                 power_set);
   ("S Test primes",           test_primes_set);
   ("S Generate primes",       generate_primes_set);
   ("S Encode / Decode msgs",  encoding_msg_set);
   ("S ciphers",               ciphers_set);
   ("S Break ciphers",         break_ciphers_set)]

let () = run_to_xml "trace_scalable_1.xml" [scalable_test_suite]
