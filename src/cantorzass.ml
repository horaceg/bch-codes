module type PolyType = Polynome.PolyType
module type FieldT = PrimeField.FieldT

module type CzType = sig
  type poly

  val cantor_zassenhaus : poly -> int -> poly
  val factorisation : poly -> int -> poly list
end

module Cantorzass (Field : FieldT) (Poly : PolyType) : CzType with type poly = Poly.t =
struct
  type poly = Poly.t

  let carac = Field.carac
  let cardinal = Field.cardinal

  let dim =
    let rec aux i k = if i > 0 then aux (i / carac) (k + 1) else k + 1 in
    aux cardinal 0

  let rec cantor_p_impair u d =
    if Poly.degre u = d
    then u
    else (
      let n = Poly.degre u in
      let r = Poly.random n in
      if Poly.degre r < 2
      then cantor_p_impair u d
      else (
        let b = Poly.powmod r ((cardinal - 1) / 2) u in
        let bmod = Poly.modulo b u in
        if bmod = Poly.one
        then cantor_p_impair u d
        else (
          let v = Poly.pgcd (Poly.sub b Poly.one) u in
          if Poly.degre v >= d then cantor_p_impair v d else cantor_p_impair u d)))

  let rec cantor_p_pair u d =
    if Poly.degre u = d
    then u
    else (
      let n = Poly.degre u in
      let r = Poly.random n in
      if Poly.degre r < 2
      then cantor_p_pair u d
      else (
        let rec somme k s =
          if k = dim
          then Poly.zero
          else (
            let smod = Poly.modulo s u in
            Poly.modulo smod u |> Poly.mul smod |> somme (k + 1) |> Poly.add smod)
        in
        let bmod = Poly.modulo (somme 0 r) u in
        if bmod = Poly.one || bmod = Poly.zero
        then cantor_p_pair u d
        else (
          let v = Poly.pgcd bmod u in
          if Poly.degre v >= d then cantor_p_pair v d else cantor_p_pair u d)))

  let cantor_zassenhaus u d =
    if carac mod 2 = 0 then cantor_p_pair u d else cantor_p_impair u d

  let rec factorisation u d =
    if Poly.degre u > 1
    then (
      let v = cantor_zassenhaus u d in
      v :: factorisation (Poly.quotient u v) d)
    else [ u ]
end
