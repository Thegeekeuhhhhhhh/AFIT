(** Generating prime bitarrays *)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
 *)
open Scalable;;
open Scalable_basic_arithmetics;;

let is_prime n =
  if (to_int n) = 2 then true else
  let max = from_int(int_of_float(sqrt(float_of_int(to_int n)))+1) in
  let rec test_premier n max =
    if (to_int max) = 1 then true else
      if (mod_b n max) = (from_int 0) then false else
        test_premier n (diff_b max (from_int 1))
  in test_premier n max;;

let mod_power x n m =
  let rec calcul_puipui x n m =
    if (to_int n) = 0 then [0;1] else if (to_int n) = 1 then (mod_b x m) else
      if (mod_b n (from_int 2)) = [0] then calcul_puipui (mod_b (mult_b (mod_b x m) (mod_b x m)) m) (quot_b n (from_int 2)) m
      else mod_b (mult_b (mod_b x m) (calcul_puipui (mod_b x m) (diff_b n (from_int 1)) m)) m
  in calcul_puipui x n m;;

let prime_mod_power x n p =
  if (((mod_b x p) = [0]) || (compare_n p n) = 1) then mod_power x n p else
    mod_power x (mod_b n (diff_n p (from_int 1))) p;;

let is_pseudo_prime p test_seq =
  if (to_int p) = 2 then true else if (is_prime p) then true else
  let rec tchoupi_a_epita p test_seq =
    match test_seq with
    |[] -> true
    |tete::reste -> if ((compare_n p tete) = 0) then true else
                      if (to_int (prime_mod_power tete (diff_n p (from_int 1)) p)) <> 1 then false else
                        tchoupi_a_epita p reste
  in tchoupi_a_epita p test_seq;;

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n =
  if (to_int n) < 2 then invalid_arg "Error [init_erathostenes_scalable] : n must be >= [0;0;1]" else
    if (to_int n) = 2 then [[0;0;1]] else
      let n = if (mod_b n [0;0;1]) = from_int 0 then (diff_b n [0;1]) else n in
      (*let depart = if (mod_b n (from_int 2)) = (from_int 0) then (diff_b n (from_int 1)) else n in*)
    let rec eratostos n liste x =
      if (to_int x) > 2 then (eratostos n (x::liste) (diff_b x [0;0;1])) else [0;0;1]::liste
    in let temp = eratostos n [[]] n in
       let rec enleve_last liste =
         match liste with
         |[] -> []
         |tete::reste -> if tete = [] then (enleve_last reste) else (tete::(enleve_last reste))
       in enleve_last temp;;

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
  if (to_int n) < 2 then invalid_arg "Error [eratosthenes_scalable] : n must be >= 2" else
    if (to_int n) = 3 then [(from_int 2);(from_int 3)] else
      let liste = init_eratosthenes n in
      let rec eratostos n liste x =
        match liste with
        |[] -> [[]] (* Not excepted to happen *)
        |tete::reste -> if is_pseudo_prime tete liste then
                          tete::(eratostos n reste x)
                        else eratostos n reste x
      in let temp = eratostos n liste (from_int 2)
         in let rec enleve_last liste =
              match liste with
              |[] -> []
              |tete::reste -> if tete = [] then (enleve_last reste) else (tete::(enleve_last reste))
            in enleve_last temp;;

(*
init_eratosthenes (from_int 581);;
eratosthenes (from_int 581);;
 *)

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = ()

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = ()

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime =
  if (compare_b limit [0;0;1]) = (-1) then invalid_arg "Error [double_primes_scalable] : limit must be >= [0;1]" else
    let rec cherche_doublette n limit =
      if compare_b n limit = 1 then [] else
        if (isprime (add_b (mult_b n [0;0;1]) [0;1])) && (isprime n) then
          ((n, (add_b (mult_b n [0;0;1]) [0;1]))::cherche_doublette (add_b n [0;1]) limit)
        else cherche_doublette (add_b n [0;1]) limit
    in cherche_doublette [0;0;1] limit;;

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  if (compare_b limit [0;0;1]) = (-1) then invalid_arg "Error [twin_primes_scalable] : limit must be >= [0;1]" else
    let rec cherche_doublette n limit =
      if compare_b n limit = 1 then [] else
        if (is_prime (add_b n [0;0;1])) && is_prime n then
          (n, (add_b n [0;0;1]))::cherche_doublette (add_b n [0;1]) limit
        else cherche_doublette (add_b n [0;1]) limit
    in cherche_doublette [0;0;1] limit;;
