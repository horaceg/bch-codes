module type FrobT =
sig
  type elt
  val zero : elt
  val un : elt
  val alpha : elt
  val carac : int
  val dim : int
  val cardinal : int
  val int_of_elt : elt -> int
  val elt_of_int : int -> elt
  val list_of_elt : elt -> int list
  val elt_of_list_int : int list -> elt
  val print : elt -> unit
  val print_table : unit -> unit
  val add : elt -> elt -> elt
  val sub : elt -> elt -> elt
  val opp : elt -> elt
  val mult : elt -> elt -> elt
  val inv : elt -> elt
  val div : elt -> elt -> elt
  val pow : elt -> int -> elt
  val random : unit -> elt
end

module Frobenius( Taille : sig val carac : int end ) : FrobT =
struct
  let rec bezout a b = match b with
    | 0 -> (a, 1, 0)
    | _ -> 
      let d, u, v = bezout b (a mod b) in
      (d, v, u - (a / b) * v)

  (** la fonction bezout manipule bien des entiers,
      			mais la fonction "inverse" prend des elt, les convertit
      			en entiers pour bezout, puis convertit le resultat 
      			de bezout en elt *)
  type elt = int
  let zero : elt = 0
  let un : elt = 1
  let alpha : elt = (if Taille.carac= 2 then 1 else if Taille.carac=3 then 2 else 3)
  let carac = Taille.carac and dim = 1
  let cardinal = carac

  let elt_of_list_int l =
    match l with
    | [] -> 0
    | [a] -> a
    | _ -> failwith "element of list int : Frob"

  let list_of_elt a =
    [a]

  let ( % ) a b =
    let x : elt = a mod b in
    if x >= zero then x else b + x 

  let elt_of_int i = match i mod carac with
    | x when x >= 0 -> let y : elt = x in y
    | x -> let y : elt = x + carac in y

  let int_of_elt (a : elt) : int = a

  let print (u:elt) = 
    print_int u;
    print_string "%";
    print_int Taille.carac

  let print_table () = ()

  let iso op_int (a : elt) (b : elt) : elt =
    let x, y = int_of_elt a, int_of_elt b in
    elt_of_int (op_int x y)

  let add a b = iso ( + ) a b
  and mult a b = iso ( * ) a b
  and sub a b = iso ( - ) a b

  let inv (a : elt) : elt = 
    let x = int_of_elt a in
    match bezout x carac with
    | 1, u, _ -> elt_of_int u
    | _ -> failwith "elt non inversible"

  let opp x = sub zero x
  let div a b = mult a (inv b)
  let pow a n = 
    let rec aux a = function
      | 0 -> un
      | k when k land 1 = 0 -> aux (mult a a) (k / 2)
      | k -> mult a (aux (mult a a) (k / 2))
    in aux a n

  let random () = elt_of_int (Random.int (carac))
end ;;