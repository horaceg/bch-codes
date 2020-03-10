module type FieldT = sig
  type t

  val zero : t
  val one : t
  val carac : int
  val dim : int
  val cardinal : int
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t
  val inv : t -> t
  val div : t -> t -> t
  val pow : t -> int -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_list : t -> int list
  val elt_of_list_int : int list -> t
  val to_string : t -> string
  val alpha : t
  val print : t -> unit
  val print_table : unit -> unit
  val random : unit -> t
end

module type TailleT = sig
  val carac : int
end

module PrimeField (Taille : TailleT) : FieldT = struct
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
    | _ -> failwith "element of list int : PrimeF"

  let to_list a = [ a ]

  let of_int i =
    match i mod carac with
    | x when x >= 0 -> x
    | x -> x + carac

  let to_int a = a

  let to_string u =
    let n = Int.to_string u in
    let p = Int.to_string Taille.carac in
    n ^ "%" ^ p

  let iso op_int a b = of_int (op_int a b)
  let add a b = iso ( + ) a b
  let mul a b = iso ( * ) a b
  let sub a b = iso ( - ) a b

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

  let is_prime p =
    if p = 2
    then true
    else (
      let rec aux n =
        match n with
        | a when a * a >= p -> true
        | a when p mod a = 0 -> false
        | _ -> false || aux (n + 2)
      in
      p mod 2 = 1 && aux 3)

  let find_primitives p =
    if not (is_prime p)
    then failwith "The cardinal of a prime field must be a prime"
    else (
      let rec order a n =
        match n with
        | 1 -> 1
        | _ -> 1 + order a (n * a mod p)
      in
      List.init (p - 1) succ |> List.filter (fun k -> order k k = p - 1))

  let alpha = find_primitives carac |> List.hd |> of_int
  let print u = to_string u |> print_string

  let print_table () =
    let rec aux i =
      match i with
      | a when a = carac -> ()
      | _ ->
        "a^" ^ Int.to_string i ^ ": " ^ to_string (pow alpha i) |> print_string;
        print_newline ();
        aux (i + 1)
    in
    aux 1
end
