open Init_values
open BriquesInit

type inf_coord = float * float
type br = inf_coord * Graphics.color
type t = br Quadtree.t

let nb_br_x = BoxInit.width /. br_width
let nb_br_y = BoxInit.height /. br_height

let potential_contact_point (bx, by) (dx, dy) =
  let norm_d = sqrt ((dx *. dx) +. (dy *. dy)) in
  let ndx, ndy = dx /. norm_d, dy /. norm_d in
  let pdx, pdy = BallInit.radius *. ndx, BallInit.radius *. ndy in
  (* marge derrière la balle pour checker quand y'a collision même si la balle va vite. C'est pas beau comme code, certes *)
  let magic_nb = 50. in
  let mdx, mdy =
    (BallInit.radius +. magic_nb) *. ndx, (BallInit.radius +. magic_nb) *. ndy
  in
  bx +. pdx, by +. pdy, bx -. mdx, by -. mdy

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

(* bool de gauche collision verticale, bool de droite collision horizontale *)
let contact_one_brick : br -> float * float -> float * float -> bool * bool =
  fun br (bx, by) (dx, dy) ->
  let (x1, y1), _ = br in
  let x2, y2 = x1 +. br_width, y1 +. br_height in
  let pbx, pby, mbx, mby = potential_contact_point (bx, by) (dx, dy) in
  let cv =
    (mbx < x1 && pbx >= x1 && by >= y1 && by <= y2)
    || (mbx > x2 && pbx <= x2 && by >= y1 && by <= y2)
  in
  let ch =
    (mby < y1 && pby >= y1 && bx >= x1 && bx <= x2)
    || (mby > y2 && pby <= y2 && bx >= x1 && bx <= x2)
  in
  cv, ch

let contact : t -> float * float -> float * float -> bool * bool =
  fun br_qtree (bx, by) (dx, dy) ->
  let nbx, nby, _, _ = potential_contact_point (bx, by) (dx, dy) in
  let mbr = Quadtree.get br_qtree (coord_to_br_inf (nbx, nby)) in
  match mbr with
  | None -> false, false
  | Some br -> contact_one_brick br (bx, by) (dx, dy)

let updated_tree br_qtree (bx, by) (dx, dy) =
  Quadtree.filter_val_count_removal br_qtree (fun br ->
    let cv, ch = contact_one_brick br (bx, by) (dx, dy) in
    not (cv || ch))

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
