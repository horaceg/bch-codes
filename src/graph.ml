(* #load "graphics.cma";;
open Graphics;;
let taille_x = 500;;
let taille_y = 500;;

open_graph (" "^(string_of_int taille_x)^"x"^(string_of_int taille_y));;

let scale sx sy offx offy (x,y)=
  ((int_of_float (x*.sx))+offx,(int_of_float (y*.sy))+offy);;

let afficher pas l scale_x scale_y offx offy=
  if l=[] then
    ()
  else
    (let (x,y) = scale scale_x scale_y offx offy (List.hd l) in
     moveto x y;
     let rec aux = function
       |[] -> ()
       |c::s -> let (x,y) = scale scale_x scale_y offx offy c in
         lineto (x+pas) y;
         moveto (x+pas) y;
         aux s in
     aux l);;

let rec graph_ln n pas=
  if n=0 then
    []
  else
    (let x = (float_of_int n)*.pas in
     (x,log x)::(graph_ln (n-1) pas));;

afficher 10 (graph_ln 10000 0.001) 100.0 100.0 0 0;;
 *)
