module type CFT = Frobenius.FieldT
module type TT = sig
  val delta : int
end

module RS (Corps : CFT) (Taille : TT) = struct
  module Cf = Corps
  module Poly = Polynome.Polynome (Cf)

  let beta = Cf.alpha

  let facteur_consecutifs =
    let rec aux = function
      | 0 -> []
      | k -> k :: aux (k - 1)
    in
    List.rev (aux (Taille.delta - 1))

  let b = 1
  let t = (Taille.delta - 1) / 2
  let beta_list = List.map (Cf.pow beta) facteur_consecutifs

  let gene =
    let x = Poly.decale Poly.un 1 in
    let rec aux = function
      | 0 -> Poly.un
      | k -> Poly.mult (Poly.sub x (Poly.monome (Cf.pow beta k) 0)) (aux (k - 1))
    in
    aux (Taille.delta - 1)

  let n = Cf.cardinal - 1
  let k = n - Poly.degre gene
  let x_n = Poly.sub (Poly.decale Poly.un n) Poly.un

  let codage l =
    let p = Poly.mult (Poly.of_list l) gene in
    Poly.modulo p x_n

  let syndrome y =
    let rec aux = function
      | [] -> []
      | e :: s -> Poly.evaluer y e :: aux s
    in
    aux beta_list

  let euclide a b =
    let rec aux u_prec u v_prec v r_prec r =
      if Poly.degre r < t
      then r, v
      else (
        let q, r_suiv = Poly.division r_prec r in
        let v_suiv = Poly.sub v_prec (Poly.mult q v) in
        let u_suiv = Poly.sub u_prec (Poly.mult q u) in
        aux u u_suiv v v_suiv r r_suiv)
    in
    aux Poly.un Poly.nul Poly.nul Poly.un a b

  let chien_algo u =
    let d = Poly.degre u in
    let u_l = Poly.to_list u in
    let e = List.hd u_l in
    let l = ref (List.tl u_l) in
    let resultat = ref [] in
    let rec aux = function
      | k when k > d -> []
      | a -> Cf.pow beta a :: aux (a + 1)
    in
    let rec somme = function
      | [] -> Cf.zero
      | a :: s -> Cf.add a (somme s)
    in
    let l_beta_prime = aux 1 in
    for i = 0 to n - 1 do
      let z = Cf.add e (somme !l) in
      if z = Cf.zero then resultat := i :: !resultat;
      l := List.map2 Cf.mult l_beta_prime !l
    done;
    !resultat

  let forney_algo racine omega lambda =
    let lambda_d = Poly.derive lambda in
    let rec aux = function
      | [] -> []
      | a :: s ->
        let x_j_inv = Cf.pow beta a in
        let o = Poly.evaluer omega x_j_inv in
        let l = Poly.evaluer lambda_d x_j_inv in
        ((n - a) mod n, Cf.opp (Cf.div o l)) :: aux s
    in
    aux racine

  let decodage_brut y = Poly.to_list (Poly.quotient y gene)

  let decodage y =
    let s = Poly.of_list (syndrome y) in
    if s = Poly.nul
    then Poly.to_list (Poly.quotient y gene)
    else (
      let omega, lambda = euclide (Poly.decale Poly.un (Taille.delta - 1)) s in
      let erreur = chien_algo lambda in
      let l =
        try forney_algo erreur omega lambda with
        | _ ->
          print_string "NON";
          []
      in
      let rec poly_erreur = function
        | [] -> Poly.nul
        | (i, c) :: s -> Poly.add (Poly.monome c i) (poly_erreur s)
      in
      let y_corr = Poly.sub y (poly_erreur l) in
      Poly.to_list (Poly.quotient y_corr gene))
end
