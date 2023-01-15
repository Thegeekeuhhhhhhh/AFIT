(** Ciphers
    Big integers based ciphers.
*)

open Z
open Z_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let bezout a b =
    let rec theoreme_mais_pas_de_pythagore a b =
      if b = zero then (a, one, zero) else
        let (diviseur, nb1, nb2) = theoreme_mais_pas_de_pythagore b (rem a b) in
        let (q, r) = div_rem a b in (diviseur, nb2, (add nb1 (neg (mul q nb2))))
    in let (a, b, c) = theoreme_mais_pas_de_pythagore a b in (b, c, a)
  in let n = mul p q and phi_de_n = (mul (pred p) (pred q)) in
    let rec cherche_e e phi =
    if gcd e phi = one then e else
      cherche_e (succ e) phi
  in let e = cherche_e (succ one) phi_de_n in
     let (d, y, z) = bezout e phi_de_n in
     ((n, e), (n, d));;

(** Encryption using RSA cryptosystem.
    @param m big integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = prime_mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m big integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = prime_mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let q = div (pred p) (succ one) in
  let rec calcule_data n p q =
    if (prime_mod_power n q p) = one then (n, p) else
      if (prime_mod_power n (mul q (succ one)) p) = one then (n, p) else
        calcule_data (succ n) p q
  in calcule_data (succ one) p q;;

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = ((prime_mod_power g (of_int 19) p), p);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = ((prime_mod_power g (of_int 19) p),
                               (rem (mul (prime_mod_power kA (of_int 19) p) msg) p));;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let bezout a b =
    let rec theoreme_mais_pas_de_pythagore a b =
      if b = zero then (a, one, zero) else
        let (diviseur, nb1, nb2) = theoreme_mais_pas_de_pythagore b (rem a b) in
        let (q, r) = div_rem a b in (diviseur, nb2, (add nb1 (neg (mul q nb2))))
    in let (a, b, c) = theoreme_mais_pas_de_pythagore a b in (b, c, a)
  in let (x, y, z) = bezout (prime_mod_power msgA (of_int 19) p) p in
     let saucisse = add (rem (mul msgB x) p) (mul (mul a zero) g)
     in if saucisse < zero then add saucisse a else saucisse;;
