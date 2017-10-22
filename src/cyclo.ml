
module type Polynome =
sig
  type poly
  val opp : poly -> poly
  val translate : int -> poly -> poly
  val decale : poly -> int -> poly
  val add : poly -> poly -> poly
  val un : poly
  val quotient : poly -> poly -> poly
end;;

module Cyclo (Poly : Polynome) =
struct
  let decomposer n =
    let rec aux k i n =
      if n > 1 then
        if n mod k = 0 then aux k (i+1) (n/k)
        else if i>0 then k::(aux (k+1) 0 n)
        else aux (k+1) 0 n
      else [k] 
    in aux 2 0 n

  let cyclo n =
    let prem = decomposer n in
    let rec prod = function
      | [] -> 1
      | k :: s -> k * (prod s) 
    in
    let m = prod prem in
    let rec aux phi = function
      | [] -> phi
      | p::s -> aux (Poly.quotient (Poly.translate (p-1) phi) phi) s
    in
    Poly.translate ((n/m)-1) (aux (Poly.add (Poly.opp Poly.un) (Poly.decale Poly.un 1)) prem) 
end;;