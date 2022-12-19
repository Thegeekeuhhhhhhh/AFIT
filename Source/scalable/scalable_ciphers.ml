(** Ciphers
    bitarrays based ciphers.
*)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
#mod_use "scalable_power.ml";;
 *)
open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)
let generate_keys_rsa p q = (* (([],[]), ([], [])) *)
  let n = (mult_b p q) and phi_de_n = (mult_b (diff_b p [0;1]) (diff_b q [0;1])) in
  let rec cherche_e e phi =
    if gcd_b e phi = [0;1] then e else
      cherche_e (add_b e [0;1]) phi
  in let e = cherche_e [0;0;1] phi_de_n in
     let (d, y, z) = bezout_b e phi_de_n in
     ((n, e), (n, d));;

(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = prime_mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = prime_mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let rec public_data_g p = (* ([], []) *)
  let q = (quot_b (diff_b p [0;1]) [0;0;1]) in
  let rec calcule_data n p q =
    if (prime_mod_power n q p) = [0;1] then (n, p) else if (prime_mod_power n (mult_b q [0;0;1]) p) = [0;1]
    then (n, p) else calcule_data (add_b n [0;1]) p q
  in calcule_data [0;0;1] p q;;

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = ((prime_mod_power g [0; 1; 1; 0; 0; 1] p), p);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = ((prime_mod_power g [0; 1; 1; 0; 0; 1] p), (mod_b (mult_b (prime_mod_power kA [0; 1; 1; 0; 0; 1] p) msg) p));;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let (x, y, z) = bezout_b (prime_mod_power msgA [0; 1; 1; 0; 0; 1] p) p in
  (add_b (mod_b (mult_b msgB x) p) (mult_b (mult_b a g) (from_int 0)));;
