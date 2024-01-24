open Init_values

module Briques = struct
    type inf_coord = float * float
    type br = inf_coord * Graphics.color
    type t = br Quadtree.t

    let br_height = 50.
    let br_width = 100.

    let nb_br_x = Box.width /. br_width
    let nb_br_y = Box.height /. br_height

    let coord_to_br (x, y) =
        let mx = floor ( x/. br_width) *. br_width in
        let my = floor ( y/. br_height) *. br_height in
        ((mx, my), (mx +. br_width, my +. br_height))

    let coord_to_br_center (x, y) =
        let mx = floor ( x/. br_width) *. br_width in
        let my = floor ( y/. br_height) *. br_height in
        (mx +. br_width /. 2., my +. br_height /. 2.)

    
    let draw_brique : br -> unit =
       fun  ((x, y), color) -> 
        Graphics.set_color color;
        Graphics.fill_rect (int_of_float x) (int_of_float y) (int_of_float br_width) (int_of_float br_height)        
        
    let draw_briques briques =
        Quadtree.iter briques draw_brique
end