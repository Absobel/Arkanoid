open Init_values
open PaletteInit

let edge_coord_float mouse_x =
  ( mouse_x -. (width /. 2.)
  , pos_y -. (height /. 2.)
  , mouse_x +. (width /. 2.)
  , pos_y +. (height /. 2.) )

let edge_coord_int mouse_x =
  ( int_of_float (mouse_x -. (width /. 2.))
  , int_of_float (pos_y -. (height /. 2.))
  , int_of_float (mouse_x +. (width /. 2.))
  , int_of_float (pos_y +. (height /. 2.)) )

let contact : float -> float * float -> float -> bool =
  fun mouse_x (bx, by) dy ->
  let by = by -. BallInit.radius in
  let x1, _, x2, y2 = edge_coord_float mouse_x in
  bx >= x1 && bx <= x2 && by >= 0. && by <= y2 && dy <= 0.

let draw_palette mouse_x =
  Graphics.set_color color;
  let x1, y1, x2, y2 = edge_coord_int mouse_x in
  Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1)
