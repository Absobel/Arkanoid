open Init_values
open PaletteInit

(** [edge_coord] retourne les coordonnées du rectangle de la palette mais float
  @param mouse_x la position de la souris
  @return les coordonnées du coin inférieur gauche et supérieur droit du rectangle de la palette en float *)
let edge_coord_float mouse_x =
  ( mouse_x -. (width /. 2.)
  , pos_y -. (height /. 2.)
  , mouse_x +. (width /. 2.)
  , pos_y +. (height /. 2.) )

(** [edge_coord] retourne les coordonnées du rectangle de la palette mais int
  @param mouse_x la position de la souris
  @return les coordonnées du coin inférieur gauche et supérieur droit du rectangle de la palette en int *)
let edge_coord_int mouse_x =
  ( int_of_float (mouse_x -. (width /. 2.))
  , int_of_float (pos_y -. (height /. 2.))
  , int_of_float (mouse_x +. (width /. 2.))
  , int_of_float (pos_y +. (height /. 2.)) )

(** [contact] retourne si la balle est en contact avec la palette
  @param mouse_x la position de la souris
  @param (bx, by) la position de la balle
  @param dy la vitesse de la balle
  @return si la balle est en contact avec la palette *)
let contact : float -> float * float -> float -> bool =
  fun mouse_x (bx, by) dy ->
  let by = by -. BallInit.radius in
  let x1, _, x2, y2 = edge_coord_float mouse_x in
  bx >= x1 && bx <= x2 && by >= 0. && by <= y2 && dy <= 0.

(** [draw_palette] dessine la palette
  @param mouse_x la position de la souris *)
let draw_palette mouse_x =
  Graphics.set_color color;
  let x1, y1, x2, y2 = edge_coord_int mouse_x in
  Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1)
