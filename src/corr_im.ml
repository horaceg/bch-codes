module Taille =
struct
  let carac = 2
  let dim = 8 (* bps *)
  let delta = 180
  let p_paq = 0.0001
  let p_pif = 0.03
  let mu = 600
  let var = 400
end;; 

module Frob = Frobenius.Frobenius(Taille);;
module Ext = Extensions.ExtensionOpt(Frob)(Taille);;
module R = Reedsolomon.RS(Ext)(Taille);;
module Mess = Bruitage.Messages(R)(Taille);;

let rec compter a b =
  match (a,b) with
  | [],[] -> 0
  | a::sa,b::sb -> 
    if a = b then
      (compter sa sb)
    else
      1 + (compter sa sb)
  | _ -> failwith "non" ;;

let completer i m =
  let rec aux = function	
    |	k when k>0 -> 0 :: aux (k-1)
    | _ -> []
  in
  m @ (aux (i - (List.length m)));;

let decomposer n =
  let rec aux = function
    | 0 -> []
    | k -> (k mod 2)::(aux (k/2)) in
  completer 8 (aux n);;


let message_of_str s =
  let l = ref[]in
  let f c =
    l := (decomposer (int_of_char c))@(!l) in
  String.iter f s;
  List.rev !l;;

let n_of_l l =
  let rec aux n e= function
    | [] -> n
    | a::s -> aux (n+a*e) (e*2) s in
  aux 0 1 l;;

let string_of_n n =
  String.make 1 (char_of_int n);;

let str_of_message l =
  let rec aux lb i = function
    |[] -> string_of_n (n_of_l lb)
    | a::s -> if i=8 then
        (string_of_n (n_of_l lb)) ^ (aux [] 0 (a::s))
      else
        (aux (a::lb) (i+1) s) in
  (aux [] 0 l);;

let list_of_string str =
  let r = ref [] in
  let f c =
    r := c :: (!r) in
  String.iter f str;
  List.rev !r;;

let bytes_of_int i =
  let rec aux e = function
    | 0 -> [e mod 256]
    | k -> (e mod 256)::(aux (e/256) (k-1)) in
  aux i 3;;

let pad_of_int i =
  if i mod 4 = 0 then
    0
  else
    4-(i mod 4);;

let output_byte_list oc l=
  List.iter (output_byte oc) l;;

let output_int_little oc i =
  output_byte_list oc (bytes_of_int i);;

let entete oc taille_x taille_y=
  let pad = pad_of_int (taille_x*3) in
  output_char oc 'B';
  output_char oc 'M';

  output_int_little oc ((taille_x)*taille_y*3+54+pad*taille_y);
  output_byte_list oc [0;0;0;0];

  output_int_little oc 54;
  output_int_little oc 40;
  output_int_little oc taille_x;
  output_int_little oc taille_y;

  output_byte_list oc [1;0;24;0];
  output_int_little oc 0;
  output_int_little oc ((taille_x)*taille_y*3+pad*taille_y);
  output_int_little oc 2835;
  output_int_little oc 2835;
  output_int_little oc 0;
  output_int_little oc 0;;

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | k -> a*(pow a (k-1));;

let input_int_little ic =
  let res = ref Int32.zero in
  for i = 0 to 3 do
    let byte = input_byte ic in
    res := Int32.logor !res (Int32.shift_left (Int32.of_int byte) (8*i))
  done;
  Int32.to_int !res;;

let read_entete ic =
  ignore (input_char ic);
  ignore (input_char ic);

  ignore (input_int_little ic);

  ignore (input_byte ic);
  ignore (input_byte ic);
  ignore (input_byte ic);
  ignore (input_byte ic);

  let offset = input_int_little ic in
  ignore (input_int_little ic);
  let taille_x = input_int_little ic in
  let taille_y = input_int_little ic in

  ignore (input_byte ic);
  ignore (input_byte ic);
  ignore (input_byte ic);
  ignore (input_byte ic);

  ignore (input_int_little ic);
  let size = input_int_little ic in
  ignore (input_int_little ic);
  ignore (input_int_little ic);
  ignore (input_int_little ic);
  ignore (input_int_little ic);

  (offset,taille_x,taille_y,(size - taille_x*taille_y*3)/taille_y);;

let open_bmp_matrix str =
  let ic = open_in_bin str in
  let (offset,taille_x,taille_y,pad) = read_entete ic in
  let resultat = Array.make_matrix taille_x taille_y (0,0,0) in
  for y=(taille_y-1) downto 0 do
    for x=0 to (taille_x-1) do
      let b = input_byte ic in
      let v = input_byte ic in
      let r = input_byte ic in
      resultat.(x).(y) <- (r,v,b);
    done;
    for i=1 to pad do
      ignore (input_byte ic);
    done;
  done;
  close_in ic;
  resultat;;

let write_bmp matrix str =
  let taille_x = Array.length matrix in
  let taille_y = Array.length matrix.(0) in
  let oc = open_out_bin str in
  let pad = pad_of_int (taille_x*3) in
  entete oc taille_x taille_y;
  for y=(taille_y-1) downto 0 do
    for x=0 to (taille_x-1) do
      let (r,v,b) = matrix.(x).(y) in
      output_byte oc b;
      output_byte oc v;
      output_byte oc r;
      if x = (taille_x-1) then
        begin
          for k=0 to (pad-1) do
            output_byte oc 0;
          done;
        end;
    done;
  done;
  close_out oc;;

let decoupe tab x y taille_x taille_y=
  let rognage = Array.make_matrix taille_y taille_x (0,0,0) in
  for xi=0 to taille_x-1 do
    for yi=0 to taille_y-1 do
      rognage.(yi).(xi) <- tab.(yi+y).(xi+x);
    done;
  done;
  rognage;;

let list_of_array tab =
  let n = Array.length tab in
  let resultat = ref []in
  for i = (n-1) downto 0 do
    resultat := tab.(i) :: !resultat;
  done;
  !resultat;;

let binaire_of_int n =
  let rec aux = function
    | 0 -> []
    | k -> (k mod 2) :: (aux (k/2)) 
  in
  let rec complet = function
    | 0 -> []
    | k -> 0::(complet (k-1)) in
  let ecr = aux n in
  ecr @ (complet (8-(List.length ecr)));;

let list_of_image im = 
  let v = Array.to_list im in
  let l = Array.to_list (Array.concat v) in
  List.concat (List.map (fun (x,y,z) -> (binaire_of_int x)@(binaire_of_int y)@(binaire_of_int z)) l);;

let image_of_list l taille_x taille_y =
  let tab = Array.make_matrix taille_x taille_y (0,0,0) in
  let rec aux x y l =
    if y = taille_y then ()
    else
    if x = taille_x then
      aux 0 (y+1) l
    else
      match l with
      | b::v::r::s -> tab.(y).(x) <- (b,v,r); aux (x+1) y s
      | _ -> ()
  in aux 0 0 l;
  tab;;


let rec groupe = function
  | [] -> []
  |a1::a2::a3::a4::a5::a6::a7::a8::s -> (a1+a2*2+a3*4+a4*8+a5*16+a6*32+a7*64+a8*128)::(groupe s)
  |_ -> [];;

module Tqdm = Tqdm.Tqdm

let coder_decoder_image image taille_y taille_x offy offx =
  let qx = taille_x / 25 in
  let qy = taille_y / 25 in
  let resultatcorrige = Array.make_matrix taille_y taille_x (0,0,0) in
  let resultatnoncorrige = Array.make_matrix taille_y taille_x (0,0,0) in
  let resultatnoncorrigev2 = Array.make_matrix taille_y taille_x (0,0,0) in
  let source = decoupe image offx offy taille_x taille_y in
  let n_erreur = ref 0 in
  let n2_erreur = ref 0 in

  let process x y =
    let newimage = (decoupe image (offx+ 25*x) (offy+ 25*y) 25 25) in
    let message = list_of_image newimage in
    let m1, m2 = (Mess.decode_double_full (Mess.codage_full message)) in
    let matc = image_of_list (groupe m1) 25 25 in
    let matnc = image_of_list (groupe m2) 25 25 in
    let matncv2 = image_of_list (groupe (Mess.bruite_paq message)) 25 25 in
    for i=0 to 24 do
      for j=0 to 24 do
        resultatcorrige.(i+ 25*y).(j+ 25*x) <- matc.(i).(j);
        resultatnoncorrige.(i+ 25*y).(j+ 25*x) <- matnc.(i).(j);
        resultatnoncorrigev2.(i+ 25*y).(j+ 25*x) <- matncv2.(i).(j);
        if matncv2.(i).(j) <> newimage.(i).(j) then
          incr n_erreur;
        if matc.(i).(j) <> newimage.(i).(j) then
          incr n2_erreur;
      done;
    done;
  in
  
  Tqdm.with_bar (qx*qy - 1) ~f:(fun tqdm->
    let i = ref 0 in
    for x=0 to qx-1 do
      for y=0 to qy-1 do
        process x y ;
        Tqdm.update tqdm (!i) ;
        incr i
      done;
    done;) ;

  write_bmp source ("./images/resultat/source.bmp");
  write_bmp resultatcorrige ("./images/resultat/corrige.bmp");
  write_bmp resultatnoncorrige ("./images/resultat/noncorrige.bmp");
  write_bmp resultatnoncorrigev2 ("./images/resultat/noncorrige_v2.bmp");
  print_newline() ;
  print_int !n_erreur;
  print_newline();
  print_int !n2_erreur;; 

let image = open_bmp_matrix "./images/perroquets.bmp";;
coder_decoder_image image 450 350 0 0;;