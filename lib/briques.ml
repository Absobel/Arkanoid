open Init_values
open BriquesInit

type inf_coord = float * float
type br = inf_coord * Graphics.color
type t = br Quadtree.t

let nb_br_x = BoxInit.width /. br_width
let nb_br_y = BoxInit.height /. br_height

let coord_to_br (x, y) =
  let mx = floor (x /. br_width) *. br_width in
  let my = floor (y /. br_height) *. br_height in
  (mx, my), (mx +. br_width, my +. br_height)

let coord_to_br_inf (x, y) =
  let mx = floor (x /. br_width) *. br_width in
  let my = floor (y /. br_height) *. br_height in
  mx, my

let coord_to_br_center (x, y) =
  let mx = floor (x /. br_width) *. br_width in
  let my = floor (y /. br_height) *. br_height in
  mx +. (br_width /. 2.), my +. (br_height /. 2.)

let contact_x_one_brick : br -> float * float -> float -> bool =
  fun ((x, y), _) (bx, by) dx ->
  let x2, y2 = x +. br_width, y +. br_height in
  let contact_left =
    bx +. BallInit.radius >= x
    && bx <= x2
    && dx >= 0.0
    && by >= y
    && by <= y2
  in
  let contact_right =
    bx -. BallInit.radius <= x2
    && bx >= x
    && dx <= 0.0
    && by >= y
    && by <= y2
  in
  contact_left || contact_right

let contact_y_one_brick : br -> float * float -> float -> bool =
  fun ((x, y), _) (bx, by) dy ->
  let x2, y2 = x +. br_width, y +. br_height in
  let contact_top =
    by +. BallInit.radius >= y
    && by <= y2
    && dy >= 0.0
    && bx >= x
    && bx <= x2
  in
  let contact_bottom =
    by -. BallInit.radius <= y2
    && by >= y
    && dy <= 0.0
    && bx >= x
    && bx <= x2
  in
  contact_top || contact_bottom

let contact_one_brick : br -> float * float -> float * float -> bool =
  fun br (bx, by) (dx, dy) ->
  contact_x_one_brick br (bx, by) dx || contact_y_one_brick br (bx, by) dy

let contact_x : t -> float * float -> float -> bool =
  fun br_qtree (bx, by) dx ->
  let mbr = Quadtree.get br_qtree (coord_to_br_inf (bx, by)) in
  match mbr with
  | None -> false
  | Some br -> contact_x_one_brick br (bx, by) dx

let contact_y : t -> float * float -> float -> bool =
  fun br_qtree (bx, by) dy ->
  let mbr = Quadtree.get br_qtree (coord_to_br_inf (bx, by)) in
  match mbr with
  | None -> false
  | Some br -> contact_y_one_brick br (bx, by) dy

let contact : t -> float * float -> float * float -> bool =
  fun br_qtree (bx, by) (dx, dy) ->
  let mbr = Quadtree.get br_qtree (coord_to_br_inf (bx, by)) in
  match mbr with
  | None -> false
  | Some br -> contact_one_brick br (bx, by) (dx, dy)

let updated_tree br_qtree (bx, by) (dx, dy) =
  Quadtree.filter_val_count_removal br_qtree (fun br ->
    not (contact_one_brick br (bx, by) (dx, dy)))

let insert_brique : br Quadtree.t -> br -> br Quadtree.t =
  fun br_qtree br ->
  let coord, color = br in
  let coord, _ = coord_to_br coord in
  Quadtree.insert br_qtree coord (coord, color)

let empty = Quadtree.empty ((0., 0.), (BoxInit.width, BoxInit.height))

let br_list_to_qtree br_list =
  let br_qtree = empty in
  List.fold_left insert_brique br_qtree br_list

let draw_brique : br -> unit =
  fun ((x, y), color) ->
  Graphics.set_color color;
  Graphics.fill_rect
    (int_of_float x)
    (int_of_float y)
    (int_of_float br_width)
    (int_of_float br_height)

let draw_briques briques = Quadtree.iter_val briques draw_brique
