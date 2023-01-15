open Alcotest
open Z
open Test_tools
open Test_z_power
open Test_z_test_primes
open Test_z_generate_primes
open Test_z_encoding_msg
open Test_z_ciphers
open Test_z_break_ciphers

let z_test_suite  = [
    ("Z Power :o",             z_power_set);
    ("Z Test primes ><",       z_test_primes_set);
    ("Z Generate primes ^^",   z_generate_primes_set);
    ("Z Ciphers :)",           z_ciphers_set);
    ("Z Break Ciphers WoW",    z_break_ciphers_set);
    (" Z Encode / Decode ^^'", z_encoding_msg_set)
  ]

let () = run_to_xml "trace_zarithing_1.xml" [z_test_suite]
