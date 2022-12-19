(** Ciphers
    Builtin integer based ciphers.
*)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;
 *)

open Builtin;;
open Basic_arithmetics;;
open Power;;

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
let encrypt_cesar k m b =
  let rec cesar_rec k m b =
    match m with
    |[] -> []
    |tete::reste -> (modulo (tete+k) b)::(cesar_rec k reste b)
  in cesar_rec k m b;;

(*
encrypt_cesar 2 [2;3;6] 10;;
encrypt_cesar 5 [2;3;6] 8;;
 *)

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
let decrypt_cesar k m b =
  let rec cesar_rec k m b =
    match m with
    |[] -> []
    |tete::reste -> (modulo (tete-k) b)::(cesar_rec k reste b)
  in cesar_rec k m b;;

(*
decrypt_cesar 2 [4;5;8] 10;;
 *)

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
 *)

let generate_keys_rsa p q =
  let n = p*q and phi_de_n = ((p-1)*(q-1)) in
  let rec cherche_e e phi =
    if gcd e phi = 1 then e else
      cherche_e (e+1) phi
  in let e = cherche_e 2 phi_de_n in
     let (d, y, z) = bezout e phi_de_n in
     ((n, e), (n, d));;

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = prime_mod_power m e n;;

(*
prime_mod_power 281237 99400891 36199;;
prime_mod_power 281237 99400891 36199003;;
prime_mod_power 281237 36199003 99400891;;
encrypt_rsa 281237 (99400891, 36199003);;
 *)

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = prime_mod_power m d n;;

(*
decrypt_rsa 70133953 (99400891, 30869683) (* 281237 *);;
 *)

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let q = (quot (p-1) 2) in
  let rec calcule_data n p q =
    if (prime_mod_power n q p) = 1 then (n, p)
    else if (prime_mod_power n (2*q) p) = 1 then (n, p)
    else calcule_data (n+1) p q
  in calcule_data 2 p q;;

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  ((prime_mod_power g 19 p), p);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = ((prime_mod_power g 19 p), (modulo ((prime_mod_power kA 19 p)*msg) p));;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let (x, y, z) = bezout (prime_mod_power msgA 19 p) p in
  ((modulo (msgB*x) p)+(g*a*0));;
