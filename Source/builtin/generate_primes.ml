(** Generating primes *)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "test_primes.ml";;
#mod_use "power.ml";;
 *)

open Builtin;;
open Basic_arithmetics;;


let is_prime n =
  let max = int_of_float(sqrt(float_of_int(n))) in
  let rec test_premier n max =
    if max = 1 then true else
      if modulo n max = 0 then false else
        test_premier n (max-1)
  in test_premier n max;;

let mod_power x n m =
  let rec puipui x n m =
    if n = 0 then 1 else if n = 1 then (modulo x m) else
      if (modulo n 2) = 0 then puipui (modulo ((modulo x m)*(modulo x m)) m) (n/2) m
      else modulo ((modulo x m) * puipui (modulo x m) (n-1) m) m
  in puipui x n m;;

let prime_mod_power x n p =
  if ((modulo x p = 0) || (n < p)) then mod_power x n p else
    mod_power x (modulo n (p-1)) p;;

let is_pseudo_prime p test_seq =
  let rec let_s_go p test_seq =
    match test_seq with
    |[] -> true
    |tete::reste -> if ((p = tete) && (is_prime p)) then true else
                      if (prime_mod_power tete (p-1) p) <> 1 then
                        false else let_s_go p reste
  in let_s_go p test_seq;;

(*
let sign x =
  if x >= 0 then 1 else (-1);;

let modulo a b =
  if b = 0 then invalid_arg "Error [Modulo] : b can not be null" else
    let b = b * sign b in
    if a = 0 then 0 else
      if a < 0 then let rec modneg a b =
                      if a >= 0 then a else modneg (a+b) b
                    in modneg a b
      else let rec modpos a b =
             if a < 0 then (a+b) else modpos (a-b) b
           in modpos a b;;
 *)

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
  if n < 2 then invalid_arg "Error [init_eratosthenes] : n must be >= 2" else
  let depart = if modulo n 2 = 0 then (n-1) else n in
  let rec eratostos n liste x =
    if x >= 2 then eratostos n (x::liste) (x-2)
    else liste
  in 2::(eratostos n [] depart);;

(*
init_eratosthenes 9999;;
init_eratosthenes 8;;
 *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
 *)

(*
let mod_power x n m =
  let rec puipui x n m =
    if n = 0 then 1 else
      modulo ((modulo x m)* puipui x (n-1) m) m
  in puipui x n m;;

let prime_mod_power x n p =
  if ((modulo x p = 0) || (n < p)) then mod_power x n p else
    let temp = modulo n (p-1) in
    mod_power x temp p;;

let is_prime n =
  let max = int_of_float(sqrt(float_of_int(n))) in
  let rec test_premier n max =
    if max = 1 then true else
      if modulo n max = 0 then false else
        test_premier n (max-1)
  in test_premier n max;;

let is_pseudo_prime p test_seq =
  let rec let_s_go p test_seq =
    match test_seq with
    |[] -> true
    |tete::reste -> if ((p = tete) && (is_prime p)) then true else
                      if (prime_mod_power tete (p-1) p) <> 1 then
                        false else let_s_go p reste
  in let_s_go p test_seq;;
 *)

let eratosthenes n =
  if n < 2 then invalid_arg "Error [eratosthenes] : n must be >= 2" else
    if n = 3 then [2; 3] else
    let liste = init_eratosthenes n in
    let rec eratostos n liste x =
        match liste with
        |[] -> []
        |tete::reste -> if is_pseudo_prime tete liste then
                          tete::(eratostos n reste x)
                        else eratostos n reste x
    in eratostos n liste 2;;

(*
eratosthenes 2;;
eratosthenes 3;;
eratosthenes 13;;
eratosthenes 145;;
eratosthenes 486;;
eratosthenes 9999;;
eratosthenes 9999;;
 *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file in
  let rec aux = function
    |[] -> close_out oc
    |tete::reste -> Printf.fprintf oc "%s " (string_of_int(tete)) ; aux reste
  in aux li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file =
  let oc = open_out file in
  let rec aux list =
    match list with
    |[] -> close_out oc
    |tete::reste -> Printf.fprintf oc "%s " (string_of_int(tete));
                    aux reste
  in aux (eratosthenes n);;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c;;

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  let ic = open_in file in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop () = match try_read () with
    |Some s -> (int_of_string(s))::(loop ())
    |None -> close_in ic;
             []
  in loop () ;;

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  if limit < 2 then invalid_arg "Error [double_primes] : limit must be >= 2" else
  let rec cherche_doublette n limit =
    if n > limit then [] else
      if isprime (2*n +1) && isprime n then ((n, 2*n + 1)::cherche_doublette (n+1) limit)
      else cherche_doublette (n+1) limit
  in cherche_doublette 2 limit;;

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  if limit < 2 then invalid_arg "Error [twin_primes] : limit must be >= 2" else
  let rec cherche_doublette n limit =
    if n > limit then [] else
      if isprime (n+2) && isprime n then ((n, n + 2)::cherche_doublette (n+1) limit)
      else cherche_doublette (n+1) limit
  in cherche_doublette 2 limit;;
