module type BaseFieldT = sig
  type t

  val zero : t
  val one : t
  val carac : int
  val dim : int
  val cardinal : int
  val to_int : t -> int
  val of_int : int -> t
  val to_list : t -> int list
  val elt_of_list_int : int list -> t
  val to_string : t -> string
  val print : t -> unit
  val print_table : unit -> unit
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t
  val inv : t -> t
  val div : t -> t -> t
  val pow : t -> int -> t
  val random : unit -> t
end

module type TailleT = sig
  val carac : int
end

module FrobeniusBase (Taille : TailleT) : BaseFieldT = struct
  let rec bezout a b =
    match b with
    | 0 -> a, 1, 0
    | _ ->
      let d, u, v = bezout b (a mod b) in
      d, v, u - (a / b * v)

  type t = int

  let zero = 0
  let one = 1
  let carac = Taille.carac
  let dim = 1
  let cardinal = carac

  let elt_of_list_int l =
    match l with
    | [] -> 0
    | [ a ] -> a
    | _ -> failwith "element of list int : Frob"

  let to_list a = [ a ]

  let ( % ) a b =
    let x = a mod b in
    if x >= 0 then x else b + x

  let of_int i =
    match i mod carac with
    | x when x >= 0 -> x
    | x -> x + carac

  let to_int a = a

  let to_string u =
    let n = Int.to_string u in
    let p = Int.to_string Taille.carac in
    n ^ "%" ^ p

  let print u = to_string u |> print_string
  let print_table () = ()
  let iso op_int a b = of_int (op_int a b)

  let add a b = iso ( + ) a b
  and mul a b = iso ( * ) a b
  and sub a b = iso ( - ) a b

  let inv a =
    match bezout a carac with
    | 1, u, _ -> of_int u
    | _ -> failwith "elt non inversible"

  let neg x = sub zero x
  let div a b = mul a (inv b)

  let pow a n =
    let rec aux a = function
      | 0 -> one
      | k when k land 1 = 0 -> aux (mul a a) (k / 2)
      | k -> mul a (aux (mul a a) (k / 2))
    in
    aux a n

  let random () = of_int (Random.int carac)
end
