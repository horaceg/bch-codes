module type PolyCZType =
sig
  type poly
  val cardinal : int
  val carac : int
  val nul : poly
  val un : poly
  val normalise : poly -> poly
  val print : poly -> unit
  val degre : poly -> int
  val add : poly -> poly -> poly
  val sub : poly -> poly -> poly
  val opp : poly -> poly
  val quotient : poly -> poly -> poly
  val modulo : poly -> poly -> poly
  val mult : poly -> poly -> poly
  val powmod : poly -> int -> poly -> poly
  val pgcd : poly -> poly -> poly
  val derive : poly -> poly
  val random : int -> poly
end

module Cantorzass (Poly : PolyCZType) =
  (struct
    type poly = Poly.poly
    let moins_un = Poly.opp Poly.un

    let carac =  Poly.carac
    let cardinal = Poly.cardinal
    let dim = 
      let rec aux i k =
        if i>0 then aux (i/(carac)) (k+1)
        else (k+1) 
      in aux cardinal 0

    let ( +: ) a b = Poly.add a b
    let ( -: ) a b = Poly.sub a b
    let ( *: ) a b = Poly.mult a b
    let ( /: ) a b = Poly.quotient a b
    let ( %: ) a b = Poly.modulo a b

    let rec cantor_p_impair u d=
      if Poly.degre u = d then u
      else
        let n = Poly.degre u in
        let r = Poly.random n in
        if Poly.degre r <2 then cantor_p_impair u d
        else
          let b = Poly.powmod r ((cardinal-1)/2) u in
          let bmod = b %: u in
          if bmod = Poly.un then cantor_p_impair u d
          else
            let v = Poly.pgcd (b -: Poly.un) u in
            if Poly.degre v >= d then cantor_p_impair v d
            else cantor_p_impair u d

    let rec cantor_p_pair u d =
      if Poly.degre u = d then u
      else
        let n = Poly.degre u in
        let r =  Poly.random n in
        if Poly.degre r<2 then cantor_p_pair u d
        else
          let rec somme k s =
            if k = dim then Poly.nul
            else
              let smod = s %: u in
              smod +: (somme (k+1) ((smod*:smod)%:u))
          in
          let bmod = (somme 0 r) %: u in
          if bmod = Poly.un || bmod = Poly.nul then cantor_p_pair u d
          else
            let v = Poly.pgcd bmod u in
            if Poly.degre v >= d then cantor_p_pair v d
            else cantor_p_pair u d

    let cantor_zassenhaus u d=
      if carac mod 2 = 0 then cantor_p_pair u d
      else cantor_p_impair u d

    let rec factorisation u d =
      if (Poly.degre u)>1 then
        let v = cantor_zassenhaus u d in
        v::(factorisation (u/:v) d)
      else [u]
  end
   :
   sig
     type poly = Poly.poly
     val cantor_zassenhaus : poly -> int -> poly
     val factorisation : poly -> int -> poly list
   end);;
