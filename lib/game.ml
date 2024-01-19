open Iterator

(* OBJECTS *)

type ball = (float * float) * (float * float)
type palette_info = int * bool
type etat = ball * int

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

module Init = struct
  let g = 500.
  let dt = 1. /. 60. (* 60 Hz *)

  let etat =
    let ball : ball = (500., 500.), (200., 200.) in
    let score = 0 in
    ball, score
  end
  
  module Ball = struct
    let radius = 10
    let color = Graphics.rgb 255 0 0
  end

module Palette = struct
  let width = 100
  let height = 10
  let color = Graphics.rgb 0 0 0
  let pos_y = 20

  let mouse_info () =
    try
      let mouse_x, _ = Graphics.mouse_pos () in
      mouse_x, Graphics.button_down ()
    with
    (* j'ai testé l'erreur arrive qu'une fois au début *)
    | Graphics.Graphic_failure "graphic screen not opened" -> 0, false

  let mouse_x () = fst (mouse_info ())
  let mouse_down () = snd (mouse_info ())

  let edge_coord mouse_x =
    ( mouse_x - (width / 2)
    , pos_y - (height / 2)
    , mouse_x + (width / 2)
    , pos_y + (height / 2) )

  let contact : int -> float * float -> float -> bool =
    fun mouse_x (bx, by) dy ->
    let bx = int_of_float bx in
    let by = int_of_float by - Ball.radius in
    let x1, _, x2, y2 = edge_coord mouse_x in
    bx >= x1 && bx <= x2 && by >= 0 && by <= y2 && dy <= 0.0

  let draw_palette () =
    let mouse_x, _ = mouse_info () in
    Graphics.set_color color;
    let x1, y1, x2, y2 = edge_coord mouse_x in
    Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1)
end

(* DRAWING FUNCTIONS *)

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
  Tick
    (lazy
      (match Flux.uncons flux with
       | None -> None
       | Some (a, fl) ->
         if cond a then Flux.uncons (f_flux a) else Some (a, unless fl cond f_flux)))

let contact_x x dx = (x > Box.supx && dx >= 0.0) || (x < Box.infx && dx <= 0.0)
let contact_high_y y dy = y > Box.supy && dy >= 0.0
let contact_low_y y dy = y < -.Box.marge && dy <= 0.0

let contact_y (x, y) dy =
  contact_high_y y dy || Palette.contact (Palette.mouse_x ()) (x, y) dy

let rebond_x x dx = if contact_x x dx then -.dx else dx
let rebond_y (x, y) dy = if contact_y (x, y) dy then -.dy else dy

(* La balle devient toute seule de plus en plus rapide due à la gravité *)
let rec update_etat : etat -> etat Flux.t =
  fun etat ->
  let ((x, y), (dx, dy)), score = etat in
  (* est juste là pour test  pour l'instant *)
  let score_flux = Flux.unfold (fun s -> Some (s, s + 1)) score in
  let dx = rebond_x x dx in
  let dy = rebond_y (x, y) dy in
  let a_flux = Flux.constant (0.0, -.Init.g) in
  let v_flux = Flux.map (fun (vx, vy) -> vx +. dx, vy +. dy) (integre Init.dt a_flux) in
  let x_flux = Flux.map (fun (nx, ny) -> nx +. x, ny +. y) (integre Init.dt v_flux) in
  let ball_flux = Flux.map2 (fun x v -> x, v) x_flux v_flux in
  unless
    (unless
       (Flux.map2 (fun b s -> b, s) ball_flux score_flux)
       (fun (((x, y), (dx, dy)), _) -> contact_x x dx || contact_y (x, y) dy)
       update_etat)
    (fun (((_, y), (_, dy)), _) -> contact_low_y y dy)
    (fun _ -> Flux.vide)
