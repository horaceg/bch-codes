module type FieldT = sig
  type t

  val zero : t
  val un : t
  val alpha : t
  val carac : int
  val dim : int
  val cardinal : int
  val to_int : t -> int
  val of_int : int -> t
  val to_list : t -> int list
  val elt_of_list_int : int list -> t
  val print : t -> unit
  val print_table : unit -> unit
  val add : t -> t -> t
  val sub : t -> t -> t
  val opp : t -> t
  val mult : t -> t -> t
  val inv : t -> t
  val div : t -> t -> t
  val pow : t -> int -> t
  val random : unit -> t
end

module Frobenius (Taille : sig
  val carac : int
end) : FieldT = struct
  let rec bezout a b =
    match b with
    | 0 -> a, 1, 0
    | _ ->
      let d, u, v = bezout b (a mod b) in
      d, v, u - (a / b * v)

  (** la fonction bezout manipule bien des entiers,
      			mais la fonction "inverse" prend des t, les convertit
      			en entiers pour bezout, puis convertit le resultat 
      			de bezout en t *)
  type t = int

  let zero = 0
  let un = 1
  let alpha = if Taille.carac = 2 then 1 else if Taille.carac = 3 then 2 else 3

  let carac = Taille.carac
  and dim = 1

  let cardinal = carac

  let elt_of_list_int l =
    match l with
    | [] -> 0
    | [ a ] -> a
    | _ -> failwith "element of list int : Frob"

  let to_list a = [ a ]

  let ( % ) a b =
    let x : t = a mod b in
    if x >= zero then x else b + x

  let of_int i =
    match i mod carac with
    | x when x >= 0 ->
      let y : t = x in
      y
    | x ->
      let y : t = x + carac in
      y

  let to_int a = a

  let print u =
    print_int u;
    print_string "%";
    print_int Taille.carac

  let print_table () = ()

  let iso op_int a b =
    let x, y = to_int a, to_int b in
    of_int (op_int x y)

  let add a b = iso ( + ) a b
  and mult a b = iso ( * ) a b
  and sub a b = iso ( - ) a b

  let inv a =
    let x = to_int a in
    match bezout x carac with
    | 1, u, _ -> of_int u
    | _ -> failwith "elt non inversible"

  let opp x = sub zero x
  let div a b = mult a (inv b)

  let pow a n =
    let rec aux a = function
      | 0 -> un
      | k when k land 1 = 0 -> aux (mult a a) (k / 2)
      | k -> mult a (aux (mult a a) (k / 2))
    in
    aux a n

  let random () = of_int (Random.int carac)
end
