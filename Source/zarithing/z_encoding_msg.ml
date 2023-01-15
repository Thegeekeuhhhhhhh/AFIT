(** Encoding Strings *)

open Z
open Z_power
open String
open Char

let int_length numero =
  let rec compte num index =
    if num = zero then index else
      compte (div num (of_int 10)) (succ index)
  in compte numero zero;;

let str_length mot =
  let rec compte str index =
    match str with
    |[] -> index
    |tete::reste -> compte reste (succ index)
  in compte mot zero;;

let convert_string_to_list_reversed str =
  let longueur = String.length str in
  let rec conversion str longueur indice =
    if indice >= longueur then [] else
      str.[to_int (longueur - indice - one)]::(conversion str longueur (succ indice))
  in conversion str (of_int longueur) zero;;

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let char_list = convert_string_to_list_reversed str in
  let rec encodestp str bits a r =
    match str with
    |[] -> zero
    |[elt] -> (add r (mul a (of_int (code (elt)))))
    |tete::reste -> encodestp reste bits (mul a (pow (succ one) bits)) (add r (mul a (of_int (code (tete)))))
  in encodestp char_list bits one zero;;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let rec count_puipui msg bits count =
    if msg < (of_int 128) then count else
      count_puipui (div msg (pow (succ one) bits)) bits (succ count)
  in let puimax = count_puipui msg bits zero in
     let rec decodestp msg bits puimax =
       if puimax = minus_one then "" else
         let temp = (pow (pow (succ one) bits) (to_int puimax)) in
         let reste = div msg temp in
         (String.make 1 (chr (to_int reste))) ^ (decodestp (add msg (neg(mul reste temp))) bits (pred puimax))
     in decodestp msg bits puimax;;
