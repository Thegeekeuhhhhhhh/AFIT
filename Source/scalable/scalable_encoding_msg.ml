(** Encoding Strings *)

(*
#mod_use "scalable.ml";;
#mod_use "scalable_basic_arithmetics.ml";;
#mod_use "scalable_power.ml";;
 *)
open Scalable
open Scalable_basic_arithmetics
open Scalable_power
open String
open Char

let str_length mot =
  let rec compte str index =
    match str with
    |[] -> index
    |tete::reste -> compte reste (index + 1)
  in compte mot 0;;

let convert_string_to_list_reversed str =
  let longueur = String.length str in
  let rec conversion str longueur indice =
    if indice >= longueur then [] else
      str.[longueur - indice - 1]::(conversion str longueur (indice + 1))
  in conversion str longueur 0;;

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
  let char_list = convert_string_to_list_reversed str in
  let rec encodestp str bits a r =
    match str with
    |[] -> (from_int 0)
    |[elt] -> (add_b (mult_b (from_int (code(elt))) a) r)
    |tete::reste -> encodestp reste bits (mult_b a (power [0;0;1] (from_int bits)))
                      (add_b (mult_b (from_int (code(tete))) a) r)
  in encodestp char_list bits (from_int 1) (from_int 0);;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let rec count_puipui msg bits count =
    if (to_int msg) < 128 then count else
      count_puipui (quot_b msg (power [0;0;1] (from_int bits))) bits (count + 1)
  in let puimax = count_puipui msg bits 0 in
     let rec decodestp msg bits puimax =
       if puimax = -1 then "" else
       let temp = (power (power [0;0;1] (from_int bits)) (from_int puimax)) in
       let reste = (quot_b msg temp) in
       (String.make 1 (chr (to_int reste))) ^ decodestp (diff_b msg (mult_b reste temp))
                                                bits (puimax - 1)
     in decodestp msg bits puimax;;
