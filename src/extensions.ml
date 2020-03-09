let rec pow_int n = function
  | 0 -> 1
  | k -> pow_int n (k - 1) * n

module type TT = sig
  val dim : int
end

module type CFT = Frobenius.FieldT

module type ExtT = sig
  include CFT

  type elt

  exception ExtraireImpossible

  module Poly : Polynome.PolyType with type elt = elt
  module Cz : Cantorzass.CzType with type poly = Poly.t
  module Cyclo : Cyclo.CycloType with type poly = Poly.t

  module Conv : sig
    val int_of_poly : Poly.t -> int
    val list_of_poly : Poly.t -> int list
    val poly_of_int : int -> Poly.t
    val completer : int list -> int list
  end

  val carac : int
  val dim : int
  val cardinal : int
  val poly_irr : Poly.t
  val to_list : t -> int list
  val elt_of_list_int : int list -> t
  val extraire : t -> elt
  val incorporer : elt -> t
  val print_table : unit -> unit
end

module ExtensionNonOpt (CorpsBase : CFT) (Taille : TT) :
  ExtT with type elt = CorpsBase.t = struct
  type elt = CorpsBase.t

  module Poly = Polynome.Polynome (CorpsBase)
  module Cz = Cantorzass.Cantorzass (CorpsBase) (Poly)
  module Cyclo = Cyclo.Cyclo (Poly)

  exception ExtraireImpossible

  type t = Poly.t

  let carac = CorpsBase.carac
  let dim = Taille.dim
  let cardinal = pow_int CorpsBase.cardinal dim

  let poly_irr =
    let poly_cyclo = Cyclo.cyclo (cardinal - 1) in
    Cz.cantor_zassenhaus poly_cyclo Taille.dim

  module Conv = struct
    let int_of_poly u =
      let rec aux acc k = function
        | [] -> acc
        | a :: s -> aux ((CorpsBase.to_int a * k) + acc) (k * CorpsBase.cardinal) s
      in
      aux 0 1 (Poly.to_list u)

    let list_of_poly u = List.map CorpsBase.to_int (Poly.to_list u)

    let poly_of_int j =
      let c = CorpsBase.cardinal in
      let rec aux = function
        | 0 -> []
        | k -> CorpsBase.of_int (k mod c) :: aux (k / c)
      in
      Poly.of_list (aux j)

    let completer m =
      let rec aux = function
        | k when k > 0 -> 0 :: aux (k - 1)
        | _ -> []
      in
      m @ aux (dim - List.length m)
  end

  let to_int a = Conv.int_of_poly a
  let to_list a = Conv.completer (Conv.list_of_poly a)
  let elt_of_list_int l = Poly.normalise (Poly.of_list (List.map CorpsBase.of_int l))
  let of_int j = Conv.poly_of_int j
  let zero = Poly.zero
  let one = Poly.one
  let alpha = Poly.decale one 1
  let add a b = Poly.add a b
  let sub a b = Poly.sub a b
  let neg a = sub zero a
  let print_table () = ()
  let mul a b = Poly.modulo (Poly.mul a b) poly_irr

  let inv a =
    let d, u, _ = Poly.euclide a poly_irr in
    if d <> Poly.one then failwith "poly non inversible" else Poly.modulo u poly_irr

  let div a b =
    let bm = inv b in
    mul a bm

  let to_string = Poly.to_string
  let print a = to_string a |> print_string
  let random () = Poly.random dim
  let pow a n = Poly.powmod a n poly_irr
  let incorporer a = if a <> CorpsBase.zero then Poly.monome a 0 else Poly.zero

  let extraire a =
    let p = a in
    if Poly.degre p > 0 then raise ExtraireImpossible else Poly.coef_constant p
end

module type CycloType = Cyclo.CycloType

module type ExtOptT = sig
  include ExtT

  module IntExt : sig
    type t

    (* val cm1 : int
       val modpos : int -> int -> int
       val add : t -> t -> t
       val neg : t -> t
       val mul : t -> t -> t *)
  end

  module Cyclo : CycloType with type poly = Poly.t

  module Conv : sig
    val table_add : IntExt.t array
    val table_mul : Poly.t array
  end
end

module ExtensionOpt (CorpsBase : CFT) (Taille : TT) :
  ExtOptT with type elt = CorpsBase.t = struct
  module ExtNonOpt = ExtensionNonOpt (CorpsBase) (Taille)
  include ExtNonOpt

  module IntExt = struct
    let cm1 = cardinal - 1

    type t =
      | Inf
      | Fini of int

    let modpos a b =
      match a mod b with
      | x when x >= 0 -> x
      | x -> b + x

    let add a b =
      match a, b with
      | Inf, _ -> Inf
      | _, Inf -> Inf
      | Fini x, Fini y -> Fini (modpos (x + y) cm1)

    let neg = function
      | Inf -> failwith "division par zero"
      | Fini x -> Fini (modpos (-x) cm1)

    let mul a b =
      match a, b with
      | Inf, _ -> Inf
      | _, Inf -> Inf
      | Fini x, Fini y -> Fini (modpos (x * y) cm1)
  end

  module Conv = struct
    include ExtNonOpt.Conv

    let tables =
      let tmul = Array.make cardinal Poly.zero in
      let tadd = Array.make cardinal IntExt.Inf in
      for i = 0 to cardinal - 2 do
        let rp = Poly.modulo (Poly.monome CorpsBase.one i) poly_irr in
        tmul.(i) <- rp;
        tadd.(int_of_poly rp) <- IntExt.Fini i
      done;
      tadd, tmul

    let table_add = fst tables
    let table_mul = snd tables
    let cycl_of_poly u = table_add.(int_of_poly u)

    let poly_of_cycl c =
      table_mul.((match c with
                 | IntExt.Inf -> cardinal - 1
                 | IntExt.Fini x -> x))
  end

  type t =
    { rep_cycl : IntExt.t
    ; rep_poly : Poly.t
    }

  let to_int a =
    let p = a.rep_poly in
    Conv.int_of_poly p

  let to_list a = Conv.completer (Conv.list_of_poly a.rep_poly)

  let elt_of_list_int l =
    let p = Poly.normalise (Poly.of_list (List.map CorpsBase.of_int l)) in
    let c = Conv.cycl_of_poly p in
    { rep_poly = p; rep_cycl = c }

  let of_int j =
    let p = Conv.poly_of_int j in
    { rep_poly = p; rep_cycl = Conv.cycl_of_poly p }

  let print_alpha_i i = Poly.print (Conv.poly_of_cycl (IntExt.Fini i))

  (*
     let print_cycle_i i =
       match Conv.cycl_of_poly (Conv.poly_of_cycl (IntExt.Fini i)) with
       | IntExt.Inf -> print_string "inf"
       | IntExt.Fini x -> print_int x *)

  let print_table () =
    for i = 0 to cardinal - 2 do
      print_alpha_i i;
      print_newline ()
    done

  let zero = { rep_cycl = IntExt.Inf; rep_poly = Poly.zero }
  let one = { rep_cycl = IntExt.Fini 0; rep_poly = Poly.one }
  let alpha = { rep_cycl = IntExt.Fini 1; rep_poly = Conv.poly_of_cycl (IntExt.Fini 1) }

  let add a b =
    let p = Poly.add a.rep_poly b.rep_poly in
    let c = Conv.cycl_of_poly p in
    { rep_cycl = c; rep_poly = p }

  let sub a b =
    let p = Poly.sub a.rep_poly b.rep_poly in
    let c = Conv.cycl_of_poly p in
    { rep_cycl = c; rep_poly = p }

  let neg a = sub zero a

  let mul a b =
    let c = IntExt.add a.rep_cycl b.rep_cycl in
    let p = Conv.poly_of_cycl c in
    { rep_cycl = c; rep_poly = p }

  let inv a =
    let c = IntExt.neg a.rep_cycl in
    let p = Conv.poly_of_cycl c in
    { rep_cycl = c; rep_poly = p }

  let div a b =
    let bm = inv b in
    mul a bm

  let to_string a =
    match a.rep_cycl with
    | IntExt.Inf -> "0"
    | IntExt.Fini k -> "a^" ^ Int.to_string k

  let print a = to_string a |> print_string

  let random () =
    let i = Random.int cardinal in
    let c = if i = cardinal - 1 then IntExt.Inf else IntExt.Fini i in
    { rep_cycl = c; rep_poly = Conv.poly_of_cycl c }

  let pow a n =
    let c = IntExt.mul a.rep_cycl (IntExt.Fini n) in
    let p = Conv.poly_of_cycl c in
    { rep_cycl = c; rep_poly = p }

  let incorporer a =
    let p = if a <> CorpsBase.zero then Poly.monome a 0 else Poly.zero in
    let c = Conv.cycl_of_poly p in
    { rep_cycl = c; rep_poly = p }

  let extraire a =
    let p = a.rep_poly in
    if Poly.degre p > 0 then raise ExtraireImpossible else Poly.coef_constant p
end
