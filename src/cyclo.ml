module type PolyType = Polynome.PolyType

module type CycloType = sig
  type poly

  val decomposer : int -> int list
  val cyclo : int -> poly
end

module Cyclo (Poly : PolyType) = struct
  type poly = Poly.t

  let decomposer n =
    let rec aux k i n =
      if n > 1
      then
        if n mod k = 0
        then aux k (i + 1) (n / k)
        else if i > 0
        then k :: aux (k + 1) 0 n
        else aux (k + 1) 0 n
      else [ k ]
    in
    aux 2 0 n

  let cyclo n =
    let prem = decomposer n in
    let rec prod = function
      | [] -> 1
      | k :: s -> k * prod s
    in
    let m = prod prem in
    let rec aux phi = function
      | [] -> phi
      | p :: s -> aux (Poly.quotient (Poly.translate (p - 1) phi) phi) s
    in
    let x = Poly.decale Poly.one 1 in
    let minus_one = Poly.neg Poly.one in
    prem |> aux (Poly.add minus_one x) |> Poly.translate ((n / m) - 1)
end
