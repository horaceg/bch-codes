let bytes_of_int i =
  let rec aux e = function
    | 0 -> [ e mod 256 ]
    | k -> (e mod 256) :: aux (e / 256) (k - 1)
  in
  aux i 3

let pad_of_int i = if i mod 4 = 0 then 0 else 4 - (i mod 4)
let output_byte_list oc l = List.iter (output_byte oc) l
let output_int_little oc i = output_byte_list oc (bytes_of_int i)

let entete oc taille_x taille_y =
  let pad = pad_of_int (taille_x * 3) in
  output_char oc 'B';
  output_char oc 'M';
  output_int_little oc ((taille_x * taille_y * 3) + 54 + (pad * taille_y));
  output_byte_list oc [ 0; 0; 0; 0 ];
  output_int_little oc 54;
  output_int_little oc 40;
  output_int_little oc taille_x;
  output_int_little oc taille_y;
  output_byte_list oc [ 1; 0; 24; 0 ];
  output_int_little oc 0;
  output_int_little oc ((taille_x * taille_y * 3) + (pad * taille_y));
  output_int_little oc 2835;
  output_int_little oc 2835;
  output_int_little oc 0;
  output_int_little oc 0

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | k -> a * pow a (k - 1)

let input_int_little ic =
  let res = ref Int32.zero in
  for i = 0 to 3 do
    let byte = input_byte ic in
    res := Int32.logor !res (Int32.shift_left (Int32.of_int byte) (8 * i))
  done;
  Int32.to_int !res

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
  offset, taille_x, taille_y, (size - (taille_x * taille_y * 3)) / taille_y

let open_bmp_matrix str =
  let ic = open_in_bin str in
  let offset, taille_x, taille_y, pad = read_entete ic in
  let resultat = Array.make_matrix taille_x taille_y (0, 0, 0) in
  for y = taille_y - 1 downto 0 do
    for x = 0 to taille_x - 1 do
      let b = input_byte ic in
      let v = input_byte ic in
      let r = input_byte ic in
      resultat.(x).(y) <- r, v, b
    done;
    for i = 1 to pad do
      ignore (input_byte ic)
    done
  done;
  close_in ic;
  resultat

let write_bmp matrix str =
  let taille_x = Array.length matrix in
  let taille_y = Array.length matrix.(0) in
  let oc = open_out_bin str in
  let pad = pad_of_int (taille_x * 3) in
  entete oc taille_x taille_y;
  for y = taille_y - 1 downto 0 do
    for x = 0 to taille_x - 1 do
      let r, v, b = matrix.(x).(y) in
      output_byte oc b;
      output_byte oc v;
      output_byte oc r;
      if x = taille_x - 1
      then
        for k = 0 to pad - 1 do
          output_byte oc 0
        done
    done
  done;
  close_out oc

;;
open_bmp_matrix "mariobros.bmp"
;;
write_bmp (open_bmp_matrix "mariobros.bmp") "copy.bmp"

;;
let matrix = Array.make_matrix 210 200 (255, 255, 255) in
write_bmp matrix "test.bmp"
