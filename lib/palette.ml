open Init_values

module Palette = struct
    open Init_values.PaletteInit
  
    let edge_coord mouse_x =
      ( mouse_x - (width / 2)
      , pos_y - (height / 2)
      , mouse_x + (width / 2)
      , pos_y + (height / 2) )
  
    let contact : float -> float * float -> float -> bool =
      fun mouse_x (bx, by) dy ->
      let bx = int_of_float bx in
      let by = int_of_float by - BallInit.radius in
      let x1, _, x2, y2 = edge_coord (int_of_float mouse_x) in
      bx >= x1 && bx <= x2 && by >= 0 && by <= y2 && dy <= 0.0
  
    let draw_palette mouse_x =
      Graphics.set_color color;
      let x1, y1, x2, y2 = edge_coord mouse_x in
      Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1)
  end