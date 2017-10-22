module type CorpsType = 
sig
  type elt
  val carac : int
  val cardinal : int
  val dim : int
  val zero : elt
  val un : elt
  val random : unit -> elt
  val print : elt -> unit
  val opp : elt -> elt
  val inv : elt -> elt
  val add : elt -> elt -> elt
  val mult : elt -> elt -> elt
  val sub : elt -> elt -> elt
  val div : elt -> elt -> elt
  val pow : elt -> int -> elt
end ;;

module type PolyFunctorType =
  functor (Corps : CorpsType) ->
  sig
    type elt = Corps.elt
    type poly
    val carac : int
    val cardinal : int
    val poly_of_list : elt list -> poly
    val list_of_poly : poly -> elt list
    val nul : poly
    val un : poly
    val coef_dom : poly -> elt
    val map : (elt -> elt) -> poly -> poly
    val evaluer : poly -> elt -> elt
    val normalise : poly -> poly
    val unitaire : poly -> poly
    val decale : poly -> int -> poly
    val monome : elt -> int -> poly
    val reciproque : poly -> poly
    val coef_constant : poly -> elt
    val degre : poly -> int
    val derive : poly -> poly
    val opp : poly -> poly
    val add : poly -> poly -> poly
    val sub : poly -> poly -> poly
    val mult : poly -> poly -> poly
    val pow : poly -> int -> poly
    val powmod : poly -> int -> poly -> poly
    val division : poly -> poly -> poly * poly
    val quotient : poly -> poly -> poly
    val modulo : poly -> poly -> poly
    val mult_par_scal : poly -> elt -> poly
    val translate : int -> poly -> poly
    val pgcd : poly -> poly -> poly
    val euclide : poly -> poly -> poly * poly * poly
    val random : int -> poly
    val print : poly -> unit
  end ;;

module Polynome : PolyFunctorType =
  functor (Corps : CorpsType) ->
  struct
    type elt = Corps.elt
    let carac = Corps.carac
    let cardinal = Corps.cardinal
    let inv = Corps.inv
    let ( +% ) = Corps.add
    let ( *% ) = Corps.mult
    let ( -% ) = Corps.sub
    let ( /% ) = Corps.div
    type poly = elt list

    let un = [Corps.un]
    let nul = []
    let map f p = List.map f p

    let normalise u =
      let rec aux = function
        | x :: s when x = Corps.zero -> aux s
        | v -> v
      in List.rev (aux (List.rev u))
    (** Attention : cette fonction ne rend pas un polynome unitaire,
        		mais enleve les zeros superflus *)

    let print a =
      let rec aux k = function
        |[] -> ()
        |a::s ->
          Corps.print a; print_string "X^"; print_int k;
          match s with
          | [] -> ()
          | _ -> print_string " + " ; aux (k+1) s
      in aux 0 a ; print_newline()

    let rec decale u e = match u, e with
      | _, 0 -> u
      | [], _ -> []
      | _ -> Corps.zero :: decale u (e - 1)

    let rec monome coef deg = match deg with
      | -1 -> []
      | 0 -> [coef]
      | _ -> Corps.zero :: monome coef (deg - 1)

    let poly_of_list a = normalise a
    let list_of_poly a = a

    let translate off u =
      let rec aux e i u =
        if i < off then Corps.zero :: aux e (i+1) u
        else match u with
          | [] -> [e]
          | a::s -> e :: aux a 0 s 
      in
      if u = nul then nul
      else aux (List.hd u) off (List.tl u) 

    let degre u = 
      let v = normalise u in
      let rec aux acc = function
        | [] -> acc
        | a :: s -> aux (succ acc) s 
      in aux (-1) (normalise v)

    let rec evaluer u x = match u with
      |[] -> Corps.zero
      | a :: q -> ( (evaluer q x) *% x ) +% a

    let rec add1 u v = match u, v with
      | [], _ -> v
      | _, [] -> u
      | a :: u', b :: v' -> a +% b :: add1 u' v'

    let add u v = normalise (add1 u v)

    let rec sub1 u v = match u, v with
      | _, [] -> u
      | [], x :: v' -> Corps.opp x :: sub1 [] v'
      | a :: u', b :: v' -> a -% b :: sub1 u' v'

    let rec sub u v = normalise (sub1 u v)

    let mult_par_scal u s = 
      if s = Corps.zero then [] else
        List.map (fun x -> x *% s) u

    let opp u = mult_par_scal u (Corps.opp Corps.un)

    let rec mult u v = match u, v with
      | [], _ -> []
      | _, [] -> []
      | [a], _ -> mult_par_scal v a
      | _, [b] -> mult_par_scal u b
      | a :: u1, b :: v1 -> a *% b ::
                            add ( add (mult_par_scal v1 a) (mult_par_scal u1 b) ) 
                              (Corps.zero :: mult u1 v1)

    let coef_dom u =
      let rec cherche = function
        | [] -> failwith "coef_dom du polynome nul"
        | e :: s when e = Corps.zero -> cherche s 
        | e :: _ -> e
      in cherche (List.rev u)

    let division (u : poly) (v : poly) =
      let inv_dom_v = inv (coef_dom v) in
      let deg_v = degre v in
      let rec aux q r =
        let deg_r = degre r in
        if deg_r < deg_v then
          (q,r)
        else
          (let dom_r = coef_dom r in
           let mq = mult_par_scal (decale v (deg_r-deg_v)) (dom_r*%inv_dom_v) in
           aux (add q (monome (dom_r*%inv_dom_v) (deg_r-deg_v))) (sub r mq))
      in
      aux [] u

    let rec div_elem u v = match u, v with
      | _, [] -> failwith "division euclidienne par le polynome nul"
      | [], _ -> ( Corps.zero, [] )
      | [a], [b] -> ( a /% b , [] )
      | a :: u1, b :: v1 ->
        let c, r1 = div_elem u1 v1 in
        (c, a -% (b *% c) :: r1 )

    let quotient u v = fst (division u v)
    let modulo u v = snd (division u v)

    let unitaire u =
      mult_par_scal u (inv (coef_dom u) )

    let rec pgcd a b =
      let rec aux a = function
        | [] -> a
        | b -> pgcd b (modulo a b)
      in unitaire (aux a b)

    let euclide a b = 
      let rec aux a b = match b with
        | [] -> ( a, un, [] )
        | _ -> 
          let (q, r) = division a b in
          let (d, u, v) = aux b r in
          ( d, v, sub u (mult q v) )
      in
      let (d, u, v) = aux a b in
      let c = inv (coef_dom d) in
      (mult_par_scal d c, mult_par_scal u c, mult_par_scal v c)

    let rec pow u = function
      | 0 -> un
      | n when n land 1 = 0 -> pow (mult u u) (n / 2)
      | n -> mult u (pow (mult u u) (n / 2))

    let rec powmod u n v = match n with
      | 0 -> un
      | n when n land 1 = 0 -> powmod (modulo (mult u u) v) (n/2) v
      | n -> modulo (mult u (powmod (modulo (mult u u) v) (n/2) v)) v

    let rec somme k a = match k with
      | 0 -> Corps.zero
      | k -> Corps.add a (somme (k-1) a)

    let derive u =
      let rec aux k = function
        | [] -> []
        | a :: s -> (somme k a) :: aux (k + 1) s 
      in match u with
      | [] -> []
      | e :: s -> normalise (aux 1 s)

    let reciproque u = List.rev u

    let coef_constant u = match u with
      | [] -> Corps.zero
      | a :: s -> a

    let random n =
      let rec aux = function
        | -1 -> []
        | k -> Corps.random () :: aux (k - 1) 
      in normalise (aux (n-1))
      (** renvoie un polynome de degré < n *)
  end;;