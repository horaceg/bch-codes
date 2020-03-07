module type BaseFieldT = sig
  type t

  val zero : t
  val un : t
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
  let un = 1
  
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

  let print u =
    print_int u;
    print_string "%";
    print_int Taille.carac

  let print_table () = ()

  let iso op_int a b =
    of_int (op_int a b)

  let add a b = iso ( + ) a b
  and mult a b = iso ( * ) a b
  and sub a b = iso ( - ) a b

  let inv a =
    match bezout a carac with
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
