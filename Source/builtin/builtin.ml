(** Tweaking OCaml builtin euclidean division

The OCaml built-in euclidian divisions operations do not follow the
standard mathematical conventions. We adapt OCaml available
primitives to suit maths conventions.

 **)

(** Sign function
    @param x integer
*)
let sign x =
  if x >= 0 then 1 else (-1);;

(** Quotient of an integer by a natural number.
    This is the quotient in euclidiant division sense.
    @param a dividend
    @param b natural number you divide by.
 *)

(*
let quot a b =
  if b = 0 then invalid_arg "b can not be null" else
    if a = 0 then 0 else
      let negatif = 1 in
      let negatif = if a < 0 then negatif*(-1) else negatif in
      let negatif = if b < 0 then negatif*(-1) else negatif in
      let result = if a < 0 then 1 else 0 in
      let a = a * sign a and b = b * sign b in
      let rec quot_petit a b q =
        (*if a = 0 then (q-1) else*)
        if a <= b then q else
          quot_petit (a-b) b (q+1)
      in let result = result + quot_petit a b 0 in
         if result < 0 then (result)*negatif else
           (result)*negatif;;
 *)

(*
let length x =
  let rec calcule x total =
    if x = 0 then total else calcule (x/10) (total + 1)
  in calcule x 0;;

let power_10 x =
  let rec letsgo x result =
    if x = 0 then result else
      letsgo (x-1) (result*10)
  in letsgo x 1;;

let find_best_elt a b =
  let rec trouve a b x =
    if (a/(power_10 ((length a)-1))) > b then (a/(power_10 ((length a)-1))) else
      if b < 10 then if b > (a/(power_10 ((length a)-1))) then a else
        (a/(power_10 ((length a)-1))) else
    if a < b then a else
    if x > b then x else
      trouve a b (x*10 + (modulo a (power_10 ((length a)-1))))
  in trouve a b 0;;
  (*
  if (length a) <> 2 then trouve a b 0
  else trouve a b (modulo a (power_10 (length a)));;*)

find_best_elt 25 4;;
find_best_elt 40000 1;;
find_best_elt 1211 10;;
 *)
let quot a b =
  if a > 0 && b > 0 then (a/b) else
  if b = 0 then invalid_arg "Error [quot] : b can not be null" else
    if a = 0 then 0 else
      let rec calcul_quotient a b q =
        if (a-b) >= 0 then calcul_quotient (a-b) b (q+1) else q
      in let resultat = if a > 0 then (calcul_quotient (a*sign a) (b*sign b) 0)*(sign b) else
                          ((calcul_quotient (a*sign a) (b*sign b) 0)+1)*(sign b)*(-1)
         in if (resultat+1)*b = a then resultat+1 else resultat;;

  (*
  if b = 0 then invalid_arg "Error [quot] : b can not be null" else
    if a = 0 then 0 else
      let rec calcul_quotient a b resultat =
        if a = 0 then resultat else
          (*let temp = a in*)
        let longueur = length a and letsgo = find_best_elt a b in
        calcul_quotient (a/10) b (resultat + (letsgo/b)*(power_10 longueur))
      in ((calcul_quotient a b 0)/100);;
   *)

(*
quot 25 4;; (* 6 *)
quot 25 (-4);; (* -6 *)
quot (-25) 4;; (* -7 *)
quot (-25) (-4);; (* 7 *)
quot (-10) 2;; (* -5 *)
quot 10 2;; (* 5 *)
quot 400000000 13;;
 *)

(** Quotient of two integers. Fully Recursive.
    General case ; explicit by-hand computations. Not efficient enough as
    it is not a low level implementation.
*)

(** Modulo of two integers.
    Following euclidean division NOT OCAML DEFAULT. Positive integer
    between 0 (included) and modulo (excluded) resulting from euclidian
    division of entry by modulo.

    OCAML DEFAULT : For negative numbers eucldean result - modulo base.

    @param a input integer
    @param b moduli integer.
 *)
let modulo a b =
  if a > 0 && b > 0 then a mod b else
  if b = 0 then invalid_arg "Error [Modulo] : b can not be null" else
    let b = b * sign b in
    if a = 0 then 0 else
      if a < 0 then let rec modneg a b =
                      if a >= 0 then a else modneg (a+b) b
                    in modneg a b
      else let rec modpos a b =
             if a < 0 then (a+b) else modpos (a-b) b
           in modpos a b;;

(*
modulo 25 4;; (* 1 *)
modulo 25 (-4);; (* 1 *)
modulo (-25) 4;; (* 3 *)
modulo (-25) (-4);; (* 3 *)
modulo (-10) 2;;
modulo 10 2;;
*)

(** Division of an integer by a natural number. NOT OCAML DEFAULT.
    Division of an integer by a non-zero integer b is the unique couple
    of integers (q, r) such that a = b*q + r and r is in [0, abs b[.
    @param a dividend
    @param b integer you divide by.
 *)

let div a b =
  let q = (quot a b) and r = (modulo a b) in
  (q, r);;
