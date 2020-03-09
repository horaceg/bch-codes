module type CFT = Frobenius.FieldT

module type TT = sig
  val n : int
end

module Decomposition (Fq : CFT) (Taille : TT) = struct
  let p = Fq.carac
  let q = Fq.cardinal
  let n = Taille.n

  let calcule_classes p q n =
    let rec reduire n r =
      match n mod p with
      | 0 -> reduire (n / p) (r + 1)
      | _ -> n, r
    in
    let n_red, r = reduire n 0 in
    (* n = p^r * n_red *)
    let m, s =
      let rec recherche qi r =
        match (qi - 1) mod n_red with
        | 0 -> r, (qi - 1) / n_red
        | _ -> recherche (q * qi) (r + 1)
      in
      recherche q 1
    in
    (* q^m = 1 mod n_red et m est minimal ; n_red * s = q^m - 1 *)
    let memo = Array.make (n_red + 1) false in
    (* memo.(j) ssi j a deja ete rencontre *)
    let classe i =
      let rec complete conjs = function
        | x when memo.(x) -> conjs
        | x ->
          memo.(x) <- true;
          complete (x :: conjs) (x * q mod n_red)
      in
      List.sort compare (complete [] i)
    in
    let rec collecte i =
      match i, memo.(i) with
      | i, _ when i = n_red -> []
      | i, true -> collecte (i + 1)
      | i, false ->
        let cl = classe i in
        cl :: collecte (i + 1)
    in
    n_red, r, m, s, collecte 0

  let n_red, r, m, s, classes_cyclo = calcule_classes p q n

  module Generateur = struct
    let n_consecutifs l =
      let rec aux i iprec prec = function
        | [] -> max i iprec
        | a :: s ->
          if a = prec + 1 then aux (i + 1) iprec a s else aux 0 (max i iprec) a s
      in
      match l with
      | [] -> 0
      | a :: s -> aux 1 0 a s

    let rec concatener l = function
      | [] -> [ l ]
      | a :: s -> (l @ a) :: concatener l s

    let rec combinaison = function
      | [] -> []
      | a :: l ->
        let c = combinaison l in
        concatener a c @ c

    let calcul_consecutifs = List.map (fun a -> n_consecutifs a, a)

    let classe_delta classes_cyclo =
      let rec aux i_prec = function
        | [] -> []
        | (i, s) :: ls when i <> i_prec -> (i, s) :: aux i ls
        | _ :: ls -> aux i_prec ls
      in
      let compare (a, _) (c, _) = Int.compare a c in
      classes_cyclo
      |> combinaison
      |> List.map (List.sort Int.compare)
      |> calcul_consecutifs
      |> List.sort compare
      |> aux (-1)

    let meilleur_ratio cl_delta n =
      let norm a b = if a > b then b - 1 else a in
      let rapport (a, b) =
        let length = List.length b in
        if a <> length
        then float_of_int (norm a (n - length)) /. float_of_int (n - length)
        else 0.0
      in
      let max_l a b = max (rapport a) (rapport b) in
      let rec aux a = function
        | [] -> 0.0
        | b :: s -> max (max_l a b) (aux b s)
      in
      match cl_delta with
      | [] -> 0.0
      | [ _ ] -> 0.0
      | _ :: b :: s -> aux b s
  end

  let classes_delta = Generateur.classe_delta classes_cyclo
  let meilleur_ratio = Generateur.meilleur_ratio classes_delta

  module Taille_ext = struct
    let dim = m
  end

  module F_qm = Extensions.ExtensionNonOpt (Fq) (Taille_ext)
  module Poly = Polynome.Polynome (F_qm)
  module PolyFq = Polynome.Polynome (Fq)

  let beta = F_qm.pow F_qm.alpha s

  let polmin cl =
    let x = Poly.monome F_qm.one 1 in
    let rec develnege = function
      | [] -> Poly.one
      | i :: s ->
        let p = Poly.sub x (Poly.monome (F_qm.pow beta i) 0) in
        Poly.mul p (develnege s)
    in
    let p_fqm = develnege cl in
    PolyFq.of_list (List.map F_qm.extraire (Poly.to_list p_fqm))

  let exposants l =
    let rec aux i iprec prec temp temp_prec = function
      | [] -> if i > iprec then temp else temp_prec
      | a :: s when a = prec + 1 -> aux (i + 1) iprec a (a :: temp) temp_prec s
      | a :: s ->
        if i > iprec then aux 0 i a [ a ] temp s else aux 0 iprec a [ a ] temp_prec s
    in
    match l with
    | [] -> failwith "vide"
    | a :: s -> aux 1 0 a [ a ] [] s

  let fact_of_delta delta =
    let rec aux k = function
      | [] -> failwith "delta introuvable" (*impossible*)
      | (i, l) :: _ when i = k -> l
      | (_, _) :: s -> aux k s
    in
    let l = aux (delta - 1) classes_delta in
    exposants l, polmin l
end
