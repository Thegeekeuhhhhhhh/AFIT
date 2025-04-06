(** Encoding Strings *)

(*
#mod_use "builtin.ml";;
#mod_use "basic_arithmetics.ml";;
#mod_use "power.ml";;
 *)
open Builtin
open Basic_arithmetics
open Power
open String
open Char

let int_length numero =
  let rec compte num index =
    if num = 0 then index else
      compte (quot num 10) (index+1)
  in compte numero 0;;

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
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let char_list = convert_string_to_list_reversed str in
  let rec encodestp str bits a r =
    match str with
    |[] -> 0
    |[elt] -> ((code (elt))*a + r)
    |tete::reste -> encodestp reste bits (a*(power 2 bits)) ((code (tete))*a + r)
  in encodestp char_list bits 1 0;;

(*
encode "Bashar" 7;;
 *)

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
    let rec count_puipui msg bits count =
    if msg < 128 then count else
      count_puipui (quot msg (power 2 bits)) bits (count + 1)
  in let puimax = count_puipui msg bits 0 in
     let rec decodestp msg bits puimax =
       if puimax = -1 then "" else
       let temp = (power (power 2 bits) puimax) in
       let reste = (quot msg temp) in
       (String.make 1 (chr reste)) ^ decodestp (msg - (reste*temp)) bits (puimax - 1)
     in decodestp msg bits puimax;;

(*
decode 2294023860466 7;;
34359738368 * 66;;
2294023860466+34359738368;;
 *)
