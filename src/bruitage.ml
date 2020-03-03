let pi = 4. *. atan 1.

module type TT =
sig
  val carac : int
  val p_paq : float
  val p_pif : float
  val mu : int
  val var : int
end

module type CT = 
sig
  val k : int
  val n : int
  module Cf :
  sig
    type t
    val zero : t
    val dim : int
    val to_list : t -> int list
    val elt_of_list_int : int list -> t
  end
  module Poly :
  sig
    type elt = Cf.t
    type t
    val list_of_poly : t -> elt list
    val poly_of_list : elt list -> t
  end
  val codage :  Cf.t list -> Poly.t
  val decodage : Poly.t -> Cf.t list
  val decodage_brut :  Poly.t -> Cf.t list
end

module Messages (Code : CT) (Taille : TT) =
struct
  open Taille
  module Cf = Code.Cf
  module Poly = Code.Poly

  let rec random = function
    | 0 -> []
    | k -> Random.int carac :: random (pred k)

  let completer i m =
    let rec aux = function	
      |	k when k>0 -> Cf.zero :: aux (k-1)
      | _ -> []
    in
    m @ (aux (i - (List.length m)))

  let completer_zero i m =
    let rec aux = function	
      |	k when k>0 -> 0 :: aux (k-1)
      | _ -> []
    in
    m @ (aux (i - (List.length m)))
  let int_list_of_mot i u =
    let l_elt = completer i u in
    List.concat (List.map Cf.to_list l_elt)	

  let elist_of_mess m = 
    let rec aux lb l i =
      match l,i with
      | l,i when i = Cf.dim+1 -> Cf.elt_of_list_int (List.rev lb) :: aux [] l 1
      | [], _ -> [Cf.elt_of_list_int (List.rev lb)]
      | a::s,i -> aux (a::lb) s (i+1) in
    aux [] m 1
  (* [0; 1; .. 1] -> [e1; ..] *)

  let mots_of_elist i l =
    let rec aux lb l k =
      match l,k with
      | l,k when k = i+1 -> List.rev lb :: aux [] l 1
      | [],_ -> [completer i (List.rev lb)]
      | a::s,k -> aux (a::lb) s (k+1) 
    in aux [] l 1
  (* l = [a1; a2; ..] -> [[a1; a2..; ai]; ..] *)

  let codage_full m =
    let lmots = mots_of_elist Code.k (elist_of_mess m) in
    List.map Code.codage lmots

  let normale () = 
    let u1, u2 = Random.float 1., Random.float 1. in
    let m, v = float_of_int mu, float_of_int var in
    int_of_float ( m +. sqrt ( (-2.) *. v *. (log u1) ) *. ( cos ( 2. *. pi *. u2 ) ) )

  let rec brouille lb k l = match k, l with
    | _,[] -> lb, []
    | 0,_ -> lb, l
    | _,a::s -> brouille (Random.int carac :: lb) (k-1) s

  let rec bruite_paq = function
    | [] -> []
    | a :: s as l ->
      let x = Random.float 1. in
      if x < p_paq then
        let nb = normale () in (* longueur de la perturbation *)
        let lb, ls = brouille [] nb l in
        lb @ (bruite_paq ls)
      else a :: bruite_paq s

  let rec bruite_pif = function
    | [] -> []
    | a :: s -> 
      let x = Random.float 1. in
      if x < p_pif then 
        Random.int carac :: bruite_pif s
      else a :: bruite_pif s

  let rec compte_erreurs l1 l2 = match l1,l2 with
      [],_ | _,[] -> 0
    | a::s, b::t when a <> b -> 1 + (compte_erreurs s t)
    | a::s, b::t -> compte_erreurs s t

  let decode_full lcodes =
    let ldecode = List.map Code.decodage lcodes in
    List.concat (List.map (int_list_of_mot Code.k) ldecode)

  let decode_double_full lcodes =
    let lmots = List.map Poly.list_of_poly lcodes in
    let l_int = List.concat (List.map (int_list_of_mot Code.n) lmots) in
    let l_bruite = bruite_paq l_int in
    let l_dec = elist_of_mess l_bruite in
    let l_mots_bruit = List.map Poly.poly_of_list (mots_of_elist Code.n l_dec) in
    let l_decode = List.map Code.decodage l_mots_bruit in
    let l_decode_brut = List.map Code.decodage_brut l_mots_bruit in
    let ilom = int_list_of_mot Code.k in
    (List.concat (List.map ilom l_decode), List.concat (List.map ilom l_decode_brut))

end 
;;
