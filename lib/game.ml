open Iterator
open Briques
open Init_values

(* OBJECTS *)

(* pos * velocity * is_launched *)
type ball = (float * float) * (float * float) * bool

module Ball = struct
  let radius = 10
  let color = Graphics.rgb 255 0 0
end

module Palette = struct
  let width = 100
  let height = 10
  let color = Graphics.rgb 0 0 0
  let pos_y = 20

  let edge_coord mouse_x =
    ( mouse_x - (width / 2)
    , pos_y - (height / 2)
    , mouse_x + (width / 2)
    , pos_y + (height / 2) )

  let contact : float -> float * float -> float -> bool =
    fun mouse_x (bx, by) dy ->
    let bx = int_of_float bx in
    let by = int_of_float by - Ball.radius in
    let x1, _, x2, y2 = edge_coord (int_of_float mouse_x) in
    bx >= x1 && bx <= x2 && by >= 0 && by <= y2 && dy <= 0.0

  let draw_palette mouse_x =
    Graphics.set_color color;
    let x1, y1, x2, y2 = edge_coord mouse_x in
    Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1)
end

type palette = float * bool
type etat = palette * ball * int * Briques.t

(* DRAWING FUNCTIONS *)

let draw_ball : ball -> unit =
  fun ((x, y), _, _) ->
  let x = int_of_float x in
  let y = int_of_float y in
  Graphics.set_color Ball.color;
  Graphics.fill_circle x y Ball.radius

(* Fonction qui intègre/somme les valeurs successives du flux *)
(* avec un pas de temps dt et une valeur initiale nulle, i.e. *)
(* acc_0 = 0; acc_{i+1} = acc_{i} + dt * flux_{i}             *)
(* paramètres:                                                *)
(* dt : float                                                 *)
(* flux : (float * float) Flux.t                              *)
let integre dt flux =
  (* valeur initiale de l'intégrateur *)
  let init = 0., 0. in
  (* fonction auxiliaire de calcul de acc_{i} + dt * flux_{i} *)
  let iter (acc1, acc2) (flux1, flux2) = acc1 +. (dt *. flux1), acc2 +. (dt *. flux2) in
  (* définition récursive du flux acc *)
  let rec acc = Tick (lazy (Some (init, Flux.map2 iter acc flux))) in
  acc

let rec unless flux cond f_flux =
  Tick
    (lazy
      (match Flux.uncons flux with
       | None -> None
       | Some (a, fl) ->
         if cond a then Flux.uncons (f_flux a) else Some (a, unless fl cond f_flux)))

let contact_x br_qtree (x, y) dx =
  (x > Box.supx && dx >= 0.0)
  || (x < Box.infx && dx <= 0.0)
  || Briques.contact_x br_qtree (x, y) dx

let contact_high_y y dy = y > Box.supy && dy >= 0.0
let contact_low_y y dy = y < -.Box.marge && dy <= 0.0

let contact_y mouse_x br_qtree (x, y) dy =
  contact_high_y y dy
  || Palette.contact mouse_x (x, y) dy
  || Briques.contact_y br_qtree (x, y) dy

let rebond_x br_qtree x dx = if contact_x br_qtree x dx then -.dx else dx

let rebond_y br_qtree mouse_x (x, y) dy =
  if contact_y mouse_x br_qtree (x, y) dy then -.dy else dy

(* GAME LOGIC *)

let update_score : int -> int Flux.t =
  fun score -> Flux.unfold (fun s -> Some (s, s + 1)) score

let update_palette () =
  Flux.unfold
    (fun () ->
      let x, _ = Graphics.mouse_pos () in
      Some ((float_of_int x, Graphics.button_down ()), ()))
    ()

let update_baballe : palette flux -> palette -> ball -> Briques.t -> ball Flux.t =
  fun palette_flux (mouse_x, mouse_down) ((x, y), (dx, dy), is_launched) br_qtree ->
  let new_is_launched = is_launched || mouse_down in
  if new_is_launched
  then (
    let contact = Palette.contact mouse_x (x, y) dy in
    let impulse =
      if contact
      then (
        let delta = x -. mouse_x in
        delta *. Init.impulse_factor)
      else 0.0
    in
    let ndx = rebond_x br_qtree (x, y) dx +. impulse in
    let ndy = rebond_y br_qtree mouse_x (x, y) dy in
    let a_flux = Flux.constant (0.0, -.Init.g) in
    let v_flux =
      Flux.map (fun (vx, vy) -> vx +. ndx, vy +. ndy) (integre Init.dt a_flux)
    in
    let x_flux = Flux.map (fun (nx, ny) -> nx +. x, ny +. y) (integre Init.dt v_flux) in
    let is_launched_flux = Flux.constant new_is_launched in
    Flux.map3 (fun x v b -> x, v, b) x_flux v_flux is_launched_flux)
  else
    Flux.map2
      (fun (mouse_x, _) dy ->
        ( (mouse_x, float_of_int (Palette.pos_y + (Ball.radius / 2)))
        , (mouse_x -. (Box.supx /. 2.), dy)
        , new_is_launched ))
      palette_flux
      (Flux.constant dy)

let update_briques : Briques.t -> ball -> Briques.t Flux.t =
  fun br_qtree ((x, y), (dx, dy), _) ->
  Flux.map
    (fun br_qtree -> Briques.updated_tree br_qtree (x, y) (dx, dy))
    (Flux.constant br_qtree)

let rec update_etat : etat -> etat Flux.t =
  fun etat ->
  let palette, ball, score, br_qtree = etat in
  let score_flux = update_score score in
  let palette_flux = update_palette () in
  let ball_flux = update_baballe palette_flux palette ball br_qtree in
  let briques_flux = update_briques br_qtree ball in
  (* modif flux *)
  let update_cond : etat -> bool =
    fun ((mouse_x, mouse_down), ((x, y), (dx, dy), is_launched), _, br_qtree) ->
    ((not is_launched) && mouse_down)
    || contact_x br_qtree (x, y) dx
    || contact_y mouse_x br_qtree (x, y) dy
  in
  let death_cond : etat -> bool =
    fun (_, ((_, y), (_, dy), _), _, _) -> contact_low_y y dy
  in
  unless
    (unless
       (Flux.map4
          (fun p b s br -> p, b, s, br)
          palette_flux
          ball_flux
          score_flux
          briques_flux)
       update_cond
       update_etat)
    death_cond
    (fun _ -> Flux.vide)
