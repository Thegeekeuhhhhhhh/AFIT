(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

 *)

let sign x =
  if x >= 0 then 1 else (-1);;

let quot a b =
  if a > 0 && b > 0 then (a/b) else
  if b = 0 then invalid_arg "Error [quot] : b can not be null" else
    if a = 0 then 0 else
      let rec calcul_quotient a b q =
        if (a-b) >= 0 then calcul_quotient (a-b) b (q+1) else q
      in let resultat = if a > 0 then (calcul_quotient (a*sign a) (b*sign b) 0)*(sign b) else
                          ((calcul_quotient (a*sign a) (b*sign b) 0)+1)*(sign b)*(-1)
         in if (resultat+1)*b = a then resultat+1 else resultat;;

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

let power x n =
  if n < 1 then 1 else
    let rec cherche_puipui x n =
      if n < 1 then 1 else
        if (modulo n 2) = 0 then cherche_puipui (x*x) (n/2)
        else (cherche_puipui (x*x) (n/2))*x
    in cherche_puipui x n;;

let maxi_bit n =
  if n = 1 then 1 else
    if n = 2 then 2 else
  let rec cherche n count =
    if power 2 count >= n then count else
      cherche n (count+1)
  in cherche n 0;;

let length_liste l =
  let rec count liste =
    match liste with
    |[] -> 0
    |tete::reste -> 1 + count reste
  in count l;;

let reverse_liste list =
  let rec travaille list final_list =
    match list with
    |[] -> final_list
    |tete::reste -> (travaille reste (tete::final_list))
  in travaille list [];;

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  if x = 0 then [] else
    let rec transform x =
      if x = 1 then [1] else
        if (modulo x 2) = 0 then 0::(transform (quot x 2))
        else 1::(transform (quot x 2))
    in if x >= 0 then 0::(transform x)
       else 1::(transform (x*sign x));;

(*
from_int 168385758548735;;
to_int [0; 1; 1; 1; 0; 0; 0; 1; 0; 1; 1; 1; 1; 1; 0; 1; 1; 0; 1; 1; 1; 1; 0; 1; 0;
 0; 0; 1; 0; 1; 1; 1; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 1; 1; 0; 0; 0; 0; 1; 0;
 0; 1; 1; 0; 1; 1; 0; 1; 1; 1; 0; 1];;
 *)

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int bA =
  if bA = [1] then 1 else
  match bA with
  |[] -> 0
  |tete::reste -> if tete = 0 then
                    let rec int_pos liste indice longueur =
                      match liste with
                      |[] -> 0
                      |tete::reste -> tete*(power 2 (longueur-indice-1)) + int_pos reste (indice+1) longueur
                    in (int_pos (reverse_liste reste) 0 (length_liste reste))
                  else
                    let rec int_pos liste indice longueur =
                      match liste with
                      |[] -> 0
                      |tete::reste -> tete*(power 2 (longueur-indice-1)) + int_pos reste (indice+1) longueur
                    in ((int_pos (reverse_liste reste) 0 (length_liste reste))*(-1));;
(*
                  else let rec int_pos liste indice longueur =
                         match liste with
                         |[] -> 0
                         |tete::reste -> tete*(power 2 (longueur-indice-1)) + int_pos reste (indice+1) longueur
                       in ((int_pos reste 0 (length_liste reste))-(power 2 ((length_liste reste))));;*)

                                         (*
                  else if reste = [0] then 0 else let rec int_neg liste indice longueur result =
                         match liste with
                         |[] -> result
                         |tete::reste -> (int_neg reste (indice+1) longueur (result - tete*(power 2 (longueur - indice))))
                       in ((int_neg reste 0 (length_liste reste) (power 2 ((length_liste reste)-1))));;
                                          *)

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA =
  let rec affiche liste =
    match liste with
    |[] -> ()
    |tete::reste -> print_int (tete);
                    affiche reste
  in affiche bA;;

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let rec compare_n nA nB =
  match (nA, nB) with
  |([], []) -> 0
  |(tete::reste, []) -> 1
  |([], tete::reste) -> (-1)
  |((tete1::reste1), (tete2::reste2)) ->
    if ((tete1 = 0 && tete2 = 0) || (tete1 = 1 && tete2 = 0)) then compare_n reste1 reste2
    else if (tete1 = 0 && tete2 = 1) then (-1) else 1;;



    (*if tete1 > tete2 then 1 else
                                           if tete2 > tete1 then (-1) el        se
                                             compare_n reste1 reste2;;*)



    (*let rec comparestp liste1 liste2 =
          match (liste1, liste2) with
          |([], []) -> true
          |(tete::reste, []) -> true
          |([], tete::reste) -> false
          |((tete1::reste1), (tete2::reste2)) -> if tete1 > tete2 then true
                                                 else if tete1 < tete2 then false
                                                 else (comparestp reste1 reste2)
                                         in let estceque = (comparestp nA nB) in
                                            let estceque = if tete1 = 0 then if tete2 = 0 then estceque
                                                                             else true else if tete2 = 0 then false else estceque in
                                            if estceque then *)

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = (compare_n nA nB) = 1;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB = (compare_n nA nB) = (-1);;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB = (((compare_n nA nB) = 1) || (compare_n nA nB = 0));;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = (((compare_n nA nB) = (-1)) || (compare_n nA nB = 0));;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB =
  let (temp1, temp2) = ((to_int bA), (to_int bB)) in
  if temp1 > temp2 then 1 else if temp1 < temp2 then (-1) else 0;;
  (*
  match (bA, bB) with
  |([], []) -> 0
  |(tete::reste, []) -> if tete = 1 then (-1) else 1
  |([], tete::reste) -> if tete = 1 then 1 else (-1)
  |((tete1::reste1), (tete2::reste2)) ->
    let rec comparestp liste1 liste2 =
      match (liste1, liste2) with
      |([], []) -> "presque"
      |(tete::reste, []) -> "true"
      |([], tete::reste) -> "false"
      |((tete1::reste1), (tete2::reste2)) ->
        if ((tete1 = 0 && tete2 = 0) || (tete1 = 1 && tete2 = 1)) then comparestp reste1 reste2
    else if (tete1 = 0 && tete2 = 1) then "false" else "true"
        (*if tete1 > tete2 then "true"
          else if tete1 < tete2 then "false"
          else (comparestp reste1 reste2)*)
    in let estceque = (comparestp reste1 reste2) in
       let estceque = if estceque = "presque" then 0 else
                        if estceque = "true" then 1 else (-1)
       in if tete1 = 0 then if tete2 = 0 then estceque else 1 else if tete2 = 1 then (-1)*estceque else (-1);;*)

         (*if tete1 = 0 then if tete2 = 0 then estceque
                                        else "true" else if tete2 = 0 then "false" else estceque in
       if estceque then*)

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = (compare_b bA bB) = 1;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB = (compare_b bA bB) = (-1);;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = (((compare_b bA bB) = 1) || ((compare_b bA bB) = 0));;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = (((compare_b bA bB) = (-1)) || ((compare_b bA bB) = 0));;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA =
  match bA with
  |[] -> 1
  |(tete::reste) -> if tete = 0 then (-1) else 1;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA =
  match bA with
  |[] -> []
  |(tete::reste) -> if tete = 1 then (0::(reste)) else bA;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB =
  let (temp1, temp2) = ((to_int nA), (to_int nB)) in
  (from_int (temp1 + temp2));;
  (*
  let reverse_list list =
    let rec travaille list final_list =
      match list with
      |[] -> final_list
      |tete::reste -> (travaille reste (tete::final_list))
    in travaille list []
  in let lst1 = reverse_list nA and lst2 = reverse_list nB in
     match (nA, nB) with
     |([], []) -> [0]
     |_ -> let rec additionne liste1 liste2 retenue =
             match (liste1, liste2) with
             |([], []) -> []
             |(tete::reste, []) -> tete::(additionne reste [] false)
             |([], tete::reste) -> tete::(additionne [] reste false)
             |((tete1::reste1), (tete2::reste2)) ->
               if retenue then if (tete1 + tete2) = 2 then 1::(additionne reste1 reste2 true) else
                                 if (tete1 + tete2) = 1 then 0::(additionne reste1 reste2 true) else
                                   1::(additionne reste1 reste2 false)
               else if (tete1 + tete2) = 2 then 0::(additionne reste1 reste2 true) else
                 if (tete1 + tete2) = 1 then 1::(additionne reste1 reste2 false) else
                   0::(additionne reste1 reste2 false)
           in additionne lst1 lst2 false;;*)

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  let (temp1, temp2) = ((to_int nA), (to_int nB)) in
  (from_int (temp1 - temp2));;

  (*(*
  let reverse_list list =
    let rec travaille list final_list =
      match list with
      |[] -> final_list
      |tete::reste -> (travaille reste (tete::final_list))
    in travaille list []
  in *)let lst1 = (*reverse_list*) nA and lst2 = (*reverse_list *)nB in
     match (nA, nB) with
     |([], []) -> [0]
     |_ -> let rec soustrait liste1 liste2 retenue =
             match (liste1, liste2) with
             |([], []) -> []
             |(tete::reste, []) -> if retenue then 0::(soustrait reste [] false)
                                   else tete::(soustrait reste [] false)
             |([], tete::reste) -> tete::(soustrait [] reste false)
             |((tete1::reste1), (tete2::reste2)) ->
               if retenue then if (tete1 - tete2) = 1 then 0::(soustrait reste1 reste2 false) else
                                 if (tete1 - tete2) = 0 then 1::(soustrait reste1 reste2 true) else
                                   0::(soustrait reste1 reste2 true)
               else if (tete1 - tete2) = 1 then 1::(soustrait reste1 reste2 false) else
                 if (tete1 - tete2) = 0 then 0::(soustrait reste1 reste2 false) else
                   1::(soustrait reste1 reste2 true)
           in soustrait lst1 lst2 false;;*)

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB =
  let (temp1, temp2) = ((to_int bA), (to_int bB)) in
  (from_int (temp1 + temp2));;

  (*
  match (bA, bB) with
  |([], []) -> [0]
  |(tete::reste, []) -> bA
  |([], tete::reste) -> bB
  |((tete1::reste1), (tete2::reste2)) -> if tete1 = 0 then if tete2 = 0 then 0::(add_n reste1 reste2)
                                                           else if compare_n reste1 reste2 = 1 then
                                                             0::(add_n reste1 reste2)
                                                           else 1::(add_n reste1 reste2)
                                         else if tete2 = 0 then if compare_n reste1 reste2 = (-1) then
                                                                  0::(add_n reste1 reste2)
                                                                else 1::(add_n reste1 reste2)
                                         else 1::(add_n reste1 reste2);;*)
  (*
  let reverse_list list =
    let rec travaille list final_list =
      match list with
      |[] -> final_list
      |tete::reste -> (travaille reste (tete::final_list))
    in travaille list []
  in let lst1 = bA and lst2 = bB in
     match (bA, bB) with
     |([], []) -> [0]
     |_ -> let rec additionne liste1 liste2 retenue =
             match (liste1, liste2) with
             |([], []) -> []
             |(tete::reste, []) -> tete::(additionne reste [] false)
             |([], tete::reste) -> tete::(additionne [] reste false)
             |((tete1::reste1), (tete2::reste2)) ->
               if retenue then if (tete1 + tete2) = 2 then 1::(additionne reste1 reste2 true) else
                                 if (tete1 + tete2) = 1 then 0::(additionne reste1 reste2 true) else
                                   1::(additionne reste1 reste2 false)
               else if (tete1 + tete2) = 2 then 0::(additionne reste1 reste2 true) else
                 if (tete1 + tete2) = 1 then 1::(additionne reste1 reste2 false) else
                   0::(additionne reste1 reste2 false)
           in additionne lst1 lst2 false;;*)

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB =
  let (temp1, temp2) = ((to_int bA), (to_int bB)) in
  (from_int (temp1 - temp2));;

(*
to_int [1];;
to_int [0;1;0;1;0;1];;
from_int 20;;
diff_b [0;1;0;1;0;1] [1];;*)
  (*
  (*let (new_a, new_b) = ((abs_b bA), (abs_b bB)) in
  match (new_a, new_b) with*)
  match (bA, bB) with
  |([], []) -> [0]
  |(tete::reste, []) -> bA
  |([], tete::reste) -> bB
  |((tete1::reste1), (tete2::reste2)) -> if tete1 = 0 then if tete2 = 0 then 0::(diff_n reste1 reste2)
                                                           else if compare_n reste1 reste2 = 1 then
                                                             0::(diff_n reste1 reste2)
                                                           else 1::(diff_n reste1 reste2)
                                         else if tete2 = 0 then if compare_n reste1 reste2 = (-1) then
                                                                  0::(diff_n reste1 reste2)
                                                                else 1::(diff_n reste1 reste2)
                                         else 0::(diff_n reste1 reste2);;*)

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d =
  let rec decale liste n final_list =
    if n = 0 then liste else
      match liste with
      |[] -> []
      |tete::reste -> (decale reste (n-1) (tete::final_list))
  in decale bA d [];;

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB =
  let (temp1, temp2) = ((to_int bA), (to_int bB)) in
  from_int (temp1*temp2);;

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB =
  let (temp1, temp2) = ((to_int bA), (to_int bB)) in
  from_int (quot temp1 temp2);;

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB =
  let (temp1, temp2) = ((to_int bA), (to_int bB)) in
  from_int (modulo temp1 temp2);;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = ((quot_b bA bB), (mod_b bA bB));;
