module type CFT = Frobenius.FieldT

module type TT = sig
  val n : int
  val delta : int
end

module BCH (Cf : CFT) (Taille : TT) = struct
  module Dec = Decomposition.Decomposition (Cf) (Taille)
  module Cf_ext = Dec.F_qm
  module Poly = Dec.Poly
  module CfPoly = Polynome.Polynome (Cf)

  let beta = Dec.beta
  let facteur_consecutifs, poly = Dec.fact_of_delta Taille.delta
  let b = List.hd facteur_consecutifs
  let beta_list = List.map (Cf_ext.pow beta) facteur_consecutifs

  let rec lst_to_surcorps = function
    | [] -> []
    | a :: s -> Cf_ext.incorporer a :: lst_to_surcorps s

  let gene = Poly.of_list (lst_to_surcorps (CfPoly.to_list poly))
  let k = Taille.n - Poly.degre gene
  let n = Taille.n
  let x_n = Poly.sub (Poly.decale Poly.one n) Poly.one

  let codage l =
    let p = Poly.mul (Poly.of_list (lst_to_surcorps l)) gene in
    Poly.modulo p x_n

  let syndrome y =
    let rec aux = function
      | [] -> []
      | e :: s -> Poly.evaluer y e :: aux s
    in
    aux beta_list

  let euclide a b =
    let rec aux u_prec u v_prec v r_prec r =
      if Poly.degre r < (Taille.delta - 1) / 2
      then r, v
      else (
        let q, r_suiv = Poly.division r_prec r in
        let v_suiv = Poly.sub v_prec (Poly.mul q v) in
        let u_suiv = Poly.sub u_prec (Poly.mul q u) in
        aux u u_suiv v v_suiv r r_suiv)
    in
    aux Poly.one Poly.zero Poly.zero Poly.one a b

  let chien_algo u =
    let d = Poly.degre u in
    let u_l = Poly.to_list u in
    let e = List.hd u_l in
    let l = ref (List.tl u_l) in
    let resultat = ref [] in
    let rec aux = function
      | k when k > d -> []
      | a -> Cf_ext.pow beta a :: aux (a + 1)
    in
    let rec somme = function
      | [] -> Cf_ext.zero
      | a :: s -> Cf_ext.add a (somme s)
    in
    let l_beta_prime = aux 1 in
    for i = 0 to Taille.n - 1 do
      let z = Cf_ext.add e (somme !l) in
      if z = Cf_ext.zero then resultat := i :: !resultat;
      l := List.map2 Cf_ext.mul l_beta_prime !l
    done;
    !resultat

  let forney_algo racine omega lambda =
    let lambda_d = Poly.derive lambda in
    let rec aux = function
      | [] -> []
      | a :: s ->
        let x_j = Cf_ext.pow beta ((n - a) mod n) in
        let x_j_inv = Cf_ext.pow beta a in
        let o = Poly.evaluer omega x_j_inv in
        let l = Poly.evaluer lambda_d x_j_inv in
        ( (n - a) mod n
        , Cf_ext.neg (Cf_ext.mul (Cf_ext.div o l) (Cf_ext.mul (Cf_ext.pow x_j_inv b) x_j))
        )
        :: aux s
    in
    aux racine

  let decode_euclide y =
    let s = Poly.of_list (syndrome y) in
    if s = Poly.zero
    then List.map Cf_ext.extraire (Poly.to_list (Poly.quotient y gene))
    else (
      let omega, lambda = euclide (Poly.decale Poly.one (Taille.delta - 1)) s in
      let erreur = chien_algo lambda in
      let l =
        List.sort
          compare
          (try forney_algo erreur omega lambda with
          | _ -> [])
      in
      let rec corriger i (m, corr) =
        match m, corr with
        | a, [] -> a
        | [], _ -> []
        | e :: s1, (a, _) :: _ when a > i -> e :: corriger (i + 1) (s1, corr)
        | e :: s1, (_, c) :: s2 -> Cf_ext.sub e c :: corriger (i + 1) (s1, s2)
      in
      let y_corr = Poly.of_list (corriger 0 (Poly.to_list y, l)) in
      let y_inv = Poly.quotient y_corr gene in
      try List.map Cf_ext.extraire (Poly.to_list y_inv) with
      | _ -> [])
end
