open Init_values
open Ball

module Briques = struct
  type inf_coord = float * float
  type br = inf_coord * Graphics.color
  type t = br Quadtree.t

  let br_height = 50.
  let br_width = 100.
  let nb_br_x = Box.width /. br_width
  let nb_br_y = Box.height /. br_height

  let coord_to_br (x, y) =
    let mx = floor (x /. br_width) *. br_width in
    let my = floor (y /. br_height) *. br_height in
    (mx, my), (mx +. br_width, my +. br_height)

  let coord_to_br_center (x, y) =
    let mx = floor (x /. br_width) *. br_width in
    let my = floor (y /. br_height) *. br_height in
    mx +. (br_width /. 2.), my +. (br_height /. 2.)

  let contact_x_one_brick : br -> float * float -> float -> bool =
    fun ((x, y), _) (bx, by) dx ->
    let x2, y2 = x +. br_width, y +. br_height in
    let contact_left =
      bx +. float_of_int Ball.radius >= x && bx <= x2 && dx >= 0.0 && by >= y && by <= y2
    in
    let contact_right =
      bx -. float_of_int Ball.radius <= x2 && bx >= x && dx <= 0.0 && by >= y && by <= y2
    in
    contact_left || contact_right

  let contact_y_one_brick : br -> float * float -> float -> bool =
    fun ((x, y), _) (bx, by) dy ->
    let x2, y2 = x +. br_width, y +. br_height in
    let contact_top =
      by +. float_of_int Ball.radius >= y && by <= y2 && dy >= 0.0 && bx >= x && bx <= x2
    in
    let contact_bottom =
      by -. float_of_int Ball.radius <= y2 && by >= y && dy <= 0.0 && bx >= x && bx <= x2
    in
    contact_top || contact_bottom

  let contact_one_brick : br -> float * float -> float -> float -> bool =
    fun br (bx, by) dx dy ->
    contact_x_one_brick br (bx, by) dx || contact_y_one_brick br (bx, by) dy

  let contact_x : t -> float * float -> float -> bool =
    fun br_qtree (bx, by) dx ->
    let mbr = Quadtree.get br_qtree (bx, by) in
    match mbr with
    | None -> false
    | Some br -> contact_x_one_brick br (bx, by) dx

  let contact_y : t -> float * float -> float -> bool =
    fun br_qtree (bx, by) dy ->
    let mbr = Quadtree.get br_qtree (bx, by) in
    match mbr with
    | None -> false
    | Some br -> contact_y_one_brick br (bx, by) dy

  let updated_tree br_qtree (bx, by) (dx, dy) =
    Quadtree.filter_val br_qtree (fun br -> not (contact_one_brick br (bx, by) dx dy))

  let draw_brique : br -> unit =
    fun ((x, y), color) ->
    Graphics.set_color color;
    Graphics.fill_rect
      (int_of_float x)
      (int_of_float y)
      (int_of_float br_width)
      (int_of_float br_height)

  let draw_briques briques = Quadtree.iter_val briques draw_brique
end
