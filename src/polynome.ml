module type CorpsType = Frobenius.FieldT

module type PolyType = sig
  type t
  type elt

  val carac : int
  val cardinal : int
  val of_list : elt list -> t
  val to_list : t -> elt list
  val nul : t
  val un : t
  val coef_dom : t -> elt
  val map : (elt -> elt) -> t -> t
  val evaluer : t -> elt -> elt
  val normalise : t -> t
  val unitaire : t -> t
  val decale : t -> int -> t
  val monome : elt -> int -> t
  val reciproque : t -> t
  val coef_constant : t -> elt
  val degre : t -> int
  val derive : t -> t
  val opp : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val pow : t -> int -> t
  val powmod : t -> int -> t -> t
  val division : t -> t -> t * t
  val quotient : t -> t -> t
  val modulo : t -> t -> t
  val mult_par_scal : t -> elt -> t
  val translate : int -> t -> t
  val pgcd : t -> t -> t
  val euclide : t -> t -> t * t * t
  val random : int -> t
  val print : t -> unit
end

 module Polynome (Corps: CorpsType): (PolyType with type elt = Corps.t) = struct
    type elt = Corps.t

    let carac = Corps.carac
    let cardinal = Corps.cardinal
    let inv = Corps.inv
    let ( +% ) = Corps.add
    let ( *% ) = Corps.mult
    let ( -% ) = Corps.sub
    let ( /% ) = Corps.div

    type t = elt list

    let un = [ Corps.un ]
    let nul = []
    let map f p = List.map f p

    (** Attention : cette fonction ne rend pas un polynome unitaire,
        		mais enleve les zeros superflus *)
    let normalise u =
      let rec aux = function
        | x :: s when x = Corps.zero -> aux s
        | v -> v
      in
      List.rev (aux (List.rev u))

    let print a =
      let rec aux k = function
        | [] -> ()
        | a :: s ->
          Corps.print a;
          print_string "X^";
          print_int k;
          (match s with
          | [] -> ()
          | _ ->
            print_string " + ";
            aux (k + 1) s)
      in
      aux 0 a;
      print_newline ()

    let rec decale u e =
      match u, e with
      | _, 0 -> u
      | [], _ -> []
      | _ -> Corps.zero :: decale u (e - 1)

    let rec monome coef deg =
      match deg with
      | -1 -> []
      | 0 -> [ coef ]
      | _ -> Corps.zero :: monome coef (deg - 1)

    let of_list a = normalise a
    let to_list a = a

    let translate off u =
      let rec aux e i u =
        if i < off
        then Corps.zero :: aux e (i + 1) u
        else (
          match u with
          | [] -> [ e ]
          | a :: s -> e :: aux a 0 s)
      in
      if u = nul then nul else aux (List.hd u) off (List.tl u)

    let degre u =
      let v = normalise u in
      let rec aux acc = function
        | [] -> acc
        | a :: s -> aux (succ acc) s
      in
      aux (-1) (normalise v)

    let rec evaluer u x =
      match u with
      | [] -> Corps.zero
      | a :: q -> (evaluer q x *% x) +% a

    let rec add1 u v =
      match u, v with
      | [], _ -> v
      | _, [] -> u
      | a :: u', b :: v' -> (a +% b) :: add1 u' v'

    let add u v = normalise (add1 u v)

    let rec sub1 u v =
      match u, v with
      | _, [] -> u
      | [], x :: v' -> Corps.opp x :: sub1 [] v'
      | a :: u', b :: v' -> (a -% b) :: sub1 u' v'

    let rec sub u v = normalise (sub1 u v)
    let mult_par_scal u s = if s = Corps.zero then [] else List.map (fun x -> x *% s) u
    let opp u = mult_par_scal u (Corps.opp Corps.un)

    let rec mult u v =
      match u, v with
      | [], _ -> []
      | _, [] -> []
      | [ a ], _ -> mult_par_scal v a
      | _, [ b ] -> mult_par_scal u b
      | a :: u1, b :: v1 ->
        (a *% b)
        :: add (add (mult_par_scal v1 a) (mult_par_scal u1 b)) (Corps.zero :: mult u1 v1)

    let coef_dom u =
      let rec cherche = function
        | [] -> failwith "coef_dom du polynome nul"
        | e :: s when e = Corps.zero -> cherche s
        | e :: _ -> e
      in
      cherche (List.rev u)

    let division (u : t) (v : t) =
      let inv_dom_v = inv (coef_dom v) in
      let deg_v = degre v in
      let rec aux q r =
        let deg_r = degre r in
        if deg_r < deg_v
        then q, r
        else (
          let dom_r = coef_dom r in
          let mq = mult_par_scal (decale v (deg_r - deg_v)) (dom_r *% inv_dom_v) in
          aux (add q (monome (dom_r *% inv_dom_v) (deg_r - deg_v))) (sub r mq))
      in
      aux [] u

    let rec div_elem u v =
      match u, v with
      | _, [] -> failwith "division euclidienne par le polynome nul"
      | [], _ -> Corps.zero, []
      | [ a ], [ b ] -> a /% b, []
      | a :: u1, b :: v1 ->
        let c, r1 = div_elem u1 v1 in
        c, (a -% (b *% c)) :: r1

    let quotient u v = fst (division u v)
    let modulo u v = snd (division u v)
    let unitaire u = mult_par_scal u (inv (coef_dom u))

    let rec pgcd a b =
      let rec aux a = function
        | [] -> a
        | b -> pgcd b (modulo a b)
      in
      unitaire (aux a b)

    let euclide a b =
      let rec aux a b =
        match b with
        | [] -> a, un, []
        | _ ->
          let q, r = division a b in
          let d, u, v = aux b r in
          d, v, sub u (mult q v)
      in
      let d, u, v = aux a b in
      let c = inv (coef_dom d) in
      mult_par_scal d c, mult_par_scal u c, mult_par_scal v c

    let rec pow u = function
      | 0 -> un
      | n when n land 1 = 0 -> pow (mult u u) (n / 2)
      | n -> mult u (pow (mult u u) (n / 2))

    let rec powmod u n v =
      match n with
      | 0 -> un
      | n when n land 1 = 0 -> powmod (modulo (mult u u) v) (n / 2) v
      | n -> modulo (mult u (powmod (modulo (mult u u) v) (n / 2) v)) v

    let rec somme k a =
      match k with
      | 0 -> Corps.zero
      | k -> Corps.add a (somme (k - 1) a)

    let derive u =
      let rec aux k = function
        | [] -> []
        | a :: s -> somme k a :: aux (k + 1) s
      in
      match u with
      | [] -> []
      | e :: s -> normalise (aux 1 s)

    let reciproque u = List.rev u

    let coef_constant u =
      match u with
      | [] -> Corps.zero
      | a :: s -> a

    (** renvoie un polynome de degr� < n *)
    let random n =
      let rec aux = function
        | -1 -> []
        | k -> Corps.random () :: aux (k - 1)
      in
      normalise (aux (n - 1))
  end
