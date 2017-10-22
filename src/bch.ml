module type CFT =
sig
  type elt
  val carac : int
  val dim : int
  val cardinal : int
  val zero : elt
  val elt_of_int : int -> elt
  val int_of_elt : elt -> int
  val random : unit -> elt
  val print : elt -> unit
  val print_table : unit -> unit
  val un : elt
  val alpha : elt
  val add : elt -> elt -> elt
  val sub : elt -> elt -> elt
  val opp : elt -> elt
  val mult : elt -> elt -> elt
  val inv : elt -> elt
  val div : elt -> elt -> elt
  val pow : elt -> int -> elt
end;;

module type TT =
sig
  val n : int
  val delta : int
end;;

module BCH (Cf : CFT) (Taille : TT)=
struct
  module Dec = Decomposition.Decomposition(Cf)(Taille)
  module Cf_ext = Dec.F_qm
  module Poly = Dec.Poly
  module CfPoly = Polynome.Polynome(Cf)

  let beta = Dec.beta
  let (facteur_consecutifs,poly) = Dec.fact_of_delta Taille.delta
  let b = List.hd facteur_consecutifs
  let beta_list = List.map (Cf_ext.pow beta) facteur_consecutifs

  let rec lst_to_surcorps = function
    |[]->[]
    |a::s -> Cf_ext.incorporer a :: lst_to_surcorps s 
  let gene = Poly.poly_of_list (lst_to_surcorps (CfPoly.list_of_poly poly))

  let k = Taille.n - Poly.degre gene
  let n = Taille.n
  let x_n = Poly.sub (Poly.decale Poly.un n) Poly.un

  let codage l =
    let p = Poly.mult (Poly.poly_of_list (lst_to_surcorps l)) gene in
    Poly.modulo p x_n

  let syndrome y =
    let rec aux = function
      |[]->[]
      |e::s -> (Poly.evaluer y e)::(aux s) 
    in aux beta_list

  let euclide a b = 
    let rec aux u_prec u v_prec v r_prec r =
      if Poly.degre r < (Taille.delta-1)/2 then (r,v)
      else
        let (q,r_suiv) = Poly.division r_prec r in
        let v_suiv = Poly.sub v_prec (Poly.mult q v) in
        let u_suiv = Poly.sub u_prec (Poly.mult q u) in
        aux u u_suiv v v_suiv r r_suiv 
    in
    aux Poly.un Poly.nul Poly.nul Poly.un a b

  let chien_algo u =
    let d = Poly.degre u in
    let u_l = Poly.list_of_poly u in
    let e = List.hd u_l in
    let l = ref (List.tl u_l) in
    let resultat = ref [] in

    let rec aux = function
      | k when k>d->[]
      |a -> (Cf_ext.pow beta a)::(aux (a+1)) in

    let rec somme = function
      |[] -> Cf_ext.zero
      |a::s -> Cf_ext.add a (somme s) in

    let l_beta_prime = aux 1 in
    for i=0 to (Taille.n-1) do
      let z = Cf_ext.add e (somme !l) in
      if z = Cf_ext.zero then
        resultat := i :: !resultat;
      l := List.map2 Cf_ext.mult l_beta_prime !l;
    done;
    !resultat

  let forney_algo racine omega lambda =
    let lambda_d = Poly.derive lambda in
    let rec aux = function
      |[]->[]
      |a::s -> 
        let x_j = Cf_ext.pow beta ((n-a) mod n) in
        let x_j_inv = Cf_ext.pow beta a in
        let o = Poly.evaluer omega x_j_inv in
        let l = Poly.evaluer lambda_d x_j_inv in
        ((n-a) mod n, Cf_ext.opp (Cf_ext.mult (Cf_ext.div o l) (Cf_ext.mult (Cf_ext.pow x_j_inv b) x_j )) ) :: aux s 
    in aux racine

  let decode_euclide y =
    let s = Poly.poly_of_list (syndrome y) in
    if s = Poly.nul then
      List.map Cf_ext.extraire (Poly.list_of_poly (Poly.quotient y gene))
    else
      let (omega,lambda) = (euclide (Poly.decale Poly.un (Taille.delta-1)) s) in
      let erreur = chien_algo lambda in
      let l = 
        List.sort compare 
          (try (forney_algo erreur omega lambda) with _ -> []) in
      let rec corriger i (m,corr) =
        match (m,corr) with
        | a,[] -> a
        | [], _ -> []
        | e::s1, (a,c)::s2 when a > i -> e::(corriger (i+1) (s1,corr))
        |  e::s1, (a, c)::s2 -> (Cf_ext.sub e c)::(corriger (i+1) (s1,s2))
      in
      let y_corr = Poly.poly_of_list (corriger 0 (Poly.list_of_poly y,l)) in
      let y_inv = Poly.quotient y_corr gene in
      try List.map Cf_ext.extraire (Poly.list_of_poly y_inv) with
      | _ -> []
end;;