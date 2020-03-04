(*
module Img = struct
    type t = int*int*int array array
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
end *)