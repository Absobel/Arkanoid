open Iterator

(* OBJECTS *)

type ball = (float * float) * (float * float)
type palette_info = int * bool
type score = int
type etat = palette_info * ball * score

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

module Init = struct
  let g = 100.
  let dt = 1. /. 60. (* 60 Hz *)

  let etat =
    let palette : palette_info = 0, false in
    let ball : ball = (500., 500.), (0., 0.) in
    let score : score = 0 in
    palette, ball, score
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
end

module Ball = struct
  let radius = 10
  let color = Graphics.rgb 255 0 0
end

(* DRAWING FUNCTIONS *)

let draw_palette : palette_info -> unit =
  fun (mouse_x, _) ->
  Graphics.set_color Palette.color;
  let x1, y1, x2, y2 = Palette.edge_coord mouse_x in
  Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1)

let draw_ball : ball -> unit =
  fun ((x, y), _) ->
  let x = int_of_float x in
  let y = int_of_float y in
  Graphics.set_color Ball.color;
  Graphics.fill_circle x y Ball.radius

(* GAME LOGIC *)

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
  match Flux.uncons flux with
  | None -> Flux.vide
  | Some (a, fl) -> if cond a then f_flux a else Flux.cons a (unless fl cond f_flux)

let contact_x x dx = (x > Box.supx && dx >= 0.0) || (x < Box.infx && dx <= 0.0)
let contact_y y dy = (y > Box.supy && dy >= 0.0) || (y < Box.infy && dy <= 0.0)
let rebond_x x dx = if contact_x x dx then -.dx else dx
let rebond_y y dy = if contact_y y dy then -.dy else dy

let ball_update : ball -> ball Flux.t =
  fun ((x, y), (dx, dy)) ->
  let a_flux = Flux.constant (0.0, -.Init.g) in
  let v_flux = Flux.map (fun (vx, vy) -> vx +. dx, vy +. dy) (integre Init.dt a_flux) in
  let x_flux = Flux.map (fun (nx, ny) -> nx +. x, ny +. y) (integre Init.dt v_flux) in
  Flux.map2 (fun x v -> x, v) x_flux v_flux
(*
   let rec ball_update : ball -> ball Flux.t =
   fun ((x, y), (dx, dy)) ->
   let a_flux = Flux.constant (0.0, -.Init.g) in
   let v_flux = Flux.map (fun (vx, vy) -> vx +. dx, vy +. dy) (integre Init.dt a_flux) in
   let x_flux = Flux.map (fun (nx, ny) -> nx +. x, ny +. y) (integre Init.dt v_flux) in
   unless
   (Flux.map2 (fun x v -> x, v) x_flux v_flux)
   (fun ((x, y), (dx, dy)) -> contact_x x dx || contact_y y dy)
   (fun ((x, y), (dx, dy)) -> ball_update ((x, y), (rebond_x x dx, rebond_y y dy)))
*)
let update_etat : etat -> etat Flux.t =
  fun (_, ball, score) ->
  let palette_flux = Input.mouse in
  let ball_flux = ball_update ball in
  let score_flux = Flux.unfold (fun s -> Some (s, s + 1)) score in
  Flux.map3 (fun p b s -> p, b, s) palette_flux ball_flux score_flux
