open Init_values
open BriquesInit

(** type d'une brique : coordonnées de son coin inférieur gauche et sa couleur *)
type br = (float * float) * Graphics.color
(** collection contenant toutes les briques *)
type t = br Quadtree.t

(** nombre de briques en longueur et en hauteur *)
let nb_br_x = BoxInit.width /. br_width
let nb_br_y = BoxInit.height /. br_height

(** [potential_contact_point] calcule le point à la surface de la balle qui va entrer en contact avec la brique 
  @param (bx, by) coordonnées de la balle
  @param (dx, dy) vecteur vitesse de la balle
  @return (pdx, pdy) coordonnées du point de contact
          (mdx, mdy) coordonnées du point de contact avec une marge derrière la balle *)
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

(** [coord_to_br] calcule les coordonnées de la brique contenant le point (x, y)
  @param (x, y) coordonnées du point
  @return (x1, y1) coordonnées du coin inférieur gauche de la brique
          (x2, y2) coordonnées du coin supérieur droit de la brique *)
let coord_to_br (x, y) =
  let mx = floor (x /. br_width) *. br_width in
  let my = floor (y /. br_height) *. br_height in
  (mx, my), (mx +. br_width, my +. br_height)

(** [contact_one_brick] calcule si la balle va entrer en contact avec la brique
  @param br brique
  @param (bx, by) coordonnées de la balle
  @param (dx, dy) vecteur vitesse de la balle
  @return (cv, ch) cv = true si la balle va entrer en contact avec la brique verticalement
                    ch = true si la balle va entrer en contact avec la brique horizontalement *)
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

(** [contact] calcule si la balle va entrer en contact avec une brique
  @param br_qtree collection de briques
  @param (bx, by) coordonnées de la balle
  @param (dx, dy) vecteur vitesse de la balle
  @return (cv, ch) cv = true si la balle va entrer en contact avec une brique verticalement
                    ch = true si la balle va entrer en contact avec une brique horizontalement *)
let contact : t -> float * float -> float * float -> bool * bool =
  fun br_qtree (bx, by) (dx, dy) ->
  let nbx, nby, _, _ = potential_contact_point (bx, by) (dx, dy) in
  let mbr = Quadtree.get br_qtree (fst (coord_to_br (nbx, nby))) in
  match mbr with
  | None -> false, false
  | Some br -> contact_one_brick br (bx, by) (dx, dy)

(** [updated_tree] calcule la collection de briques en prenant en compte d'éventuelles collisions de la balle avec les briques
  @param br_qtree collection de briques
  @param (bx, by) coordonnées de la balle
  @param (dx, dy) vecteur vitesse de la balle
  @return br_qtree collection de briques mise à jour *)
let updated_tree br_qtree (bx, by) (dx, dy) =
  Quadtree.filter_val_count_removal br_qtree (fun br ->
    let cv, ch = contact_one_brick br (bx, by) (dx, dy) in
    not (cv || ch))

(** [insert_brique] insère une brique dans la collection de briques
  @param br_qtree collection de briques
  @param br brique à insérer
  @return br_qtree collection de briques mise à jour *)
let insert_brique : br Quadtree.t -> br -> br Quadtree.t =
  fun br_qtree br ->
  let coord, color = br in
  let coord, _ = coord_to_br coord in
  Quadtree.insert br_qtree coord (coord, color)

(** [empty] collection de briques vide *)
let empty = Quadtree.empty ((0., 0.), (BoxInit.width, BoxInit.height))

(** [br_list_to_qtree] calcule la collection de briques à partir d'une liste de briques
  @param br_list liste de briques
  @return br_qtree collection de briques *)
let br_list_to_qtree br_list =
  let br_qtree = empty in
  List.fold_left insert_brique br_qtree br_list

(** [draw_brique] dessine une brique
  @param br brique à dessiner *)
let draw_brique : br -> unit =
  fun ((x, y), color) ->
  Graphics.set_color color;
  Graphics.fill_rect
    (int_of_float x)
    (int_of_float y)
    (int_of_float br_width)
    (int_of_float br_height)

(** [draw_briques] dessine toutes les briques
  @param briques collection de briques *)
let draw_briques briques = Quadtree.iter_val briques draw_brique
