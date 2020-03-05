let binaire_of_int n =
  let rec aux = function
    | 0 -> []
    | k -> (k mod 2) :: (aux (k/2)) 
    in
  let rec complet = function
    | 0 -> []
    | k -> 0::(complet (k-1)) 
    in
  let ecr = aux n in
  ecr @ (complet (8-(List.length ecr)))

(* 
module type ImgType = sig
    type t
    val load : string -> t
    val to_array : t -> (int*int*int) array array
end
*)

module Img = struct
    type t = (int*int*int) array array
    let load path = 
        let img = OImages.rgb24 (OImages.load path []) in
        let arr = Array.make_matrix img#height img#width (0, 0, 0) in
        for i = 0 to img#height - 1 do
            for j = 0 to img#width - 1 do
                let rgb = img#get j i in
                arr.(i).(j) <- (rgb.r, rgb.g, rgb.b)
            done
        done;
        arr
    
    let to_array img = img

    let decoupe tab x y taille_x taille_y=
        let rognage = Array.make_matrix taille_y taille_x (0,0,0) in
        for xi=0 to taille_x-1 do
            for yi=0 to taille_y-1 do
            rognage.(yi).(xi) <- tab.(yi+y).(xi+x);
            done;
        done;
        rognage
     
    let of_list l taille_x taille_y =
        let tab = Array.make_matrix taille_x taille_y (0,0,0) in
        let rec aux x y l =
            if y = taille_y then 
                ()
            else if x = taille_x then
                aux 0 (y+1) l
            else match l with
                | b::v::r::s -> tab.(y).(x) <- (b,v,r); aux (x+1) y s
                | _ -> ()
        in aux 0 0 l;
        tab
        
    let to_list im = 
        let v = Array.to_list im in
        let l = Array.to_list (Array.concat v) in
        List.concat (List.map (fun (x,y,z) -> (binaire_of_int x)@(binaire_of_int y)@(binaire_of_int z)) l)

end
