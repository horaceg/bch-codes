let src_path = ref "images/perroquets.bmp"
let output_dir = ref "images/resultat/"

let usage = "usage: " ^ Sys.argv.(0) ^ " [-i string] [-o string]"

let speclist = [
    ("-i", Arg.Set_string src_path, "input image location");
    ("-o", Arg.Set_string output_dir, "output directory");
  ]

let () =
  (* Read the arguments *)
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage ;
    
    let image = Correct.open_bmp_matrix !src_path in
    let tuple = Correct.coder_decoder_image_2 image in
    Correct.write_outputs tuple !output_dir
;;