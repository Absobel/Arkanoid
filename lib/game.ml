open Iterator
open Init_values

(* OBJECTS *)

module Box = struct
  open Init_values.BoxInit

  let infx = 0. +. marge
  let infy = 0. +. marge
  let supx = width -. marge
  let supy = height -. marge
end

type ball = (float * float) * (float * float) * bool
type palette = float * float * bool
type etat = palette * ball * int * (Briques.t * int)

(* DRAWING FUNCTIONS *)

let draw_ball : ball -> unit =
  fun ((x, y), _, _) ->
  let x = int_of_float x in
  let y = int_of_float y in
  Graphics.set_color BallInit.color;
  Graphics.fill_circle x y (int_of_float BallInit.radius)

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

let contact_x br_qtree (x, y) (dx, dy) =
  (x > Box.supx && dx >= 0.0)
  || (x < Box.infx && dx <= 0.0)
  || fst (Briques.contact br_qtree (x, y) (dx, dy))

let contact_y mouse_x br_qtree (x, y) (dx, dy) =
  (y > Box.supy && dy >= 0.0)
  || Palette.contact mouse_x (x, y) dy
  || snd (Briques.contact br_qtree (x, y) (dx, dy))

let rebond_x br_qtree p (dx, dy) = if contact_x br_qtree p (dx, dy) then -.dx else dx

let rebond_y br_qtree mouse_x p (dx, dy) =
  if contact_y mouse_x br_qtree p (dx, dy) then -.dy else dy

(* GAME LOGIC *)

let update_score score nb_br_touched =
  Flux.constant (score + (nb_br_touched * BriquesInit.score_per_br))

let update_palette () =
  Flux.unfold
    (fun prev_x ->
      let x, _ = Graphics.mouse_pos () in
      let x = float_of_int x in
      let dx = (x -. prev_x) /. PhysicsInit.dt in
      Some ((x, dx, Graphics.button_down ()), x))
    0.0

let update_baballe : palette flux -> palette -> ball -> Briques.t -> ball Flux.t =
  fun palette_flux
    (mouse_x, mouse_dx, mouse_down)
    ((x, y), (dx, dy), is_launched)
    br_qtree ->
  let new_is_launched = is_launched || mouse_down in
  if new_is_launched
  then (
    let contact = Palette.contact mouse_x (x, y) dy in
    let impulse = if contact then mouse_dx *. PhysicsInit.impulse_factor else 0.0 in
    let ndx = rebond_x br_qtree (x, y) (dx, dy) +. impulse in
    let ndy = rebond_y br_qtree mouse_x (x, y) (dx, dy) in
    let a_flux = Flux.constant (0.0, -.PhysicsInit.g) in
    let v_flux =
      Flux.map (fun (vx, vy) -> vx +. ndx, vy +. ndy) (integre PhysicsInit.dt a_flux)
    in
    let x_flux =
      Flux.map (fun (nx, ny) -> nx +. x, ny +. y) (integre PhysicsInit.dt v_flux)
    in
    let is_launched_flux = Flux.constant new_is_launched in
    Flux.map3 (fun x v b -> x, v, b) x_flux v_flux is_launched_flux)
  else
    Flux.map2
      (fun (mouse_x, mouse_dx, _) dy ->
        ( (mouse_x, PaletteInit.pos_y +. (BallInit.radius /. 2.))
        , (mouse_dx, dy)
        , new_is_launched ))
      palette_flux
      (Flux.constant dy)

let update_briques : Briques.t -> ball -> (Briques.t * int) Flux.t =
  fun br_qtree ((x, y), (dx, dy), _) ->
  Flux.map
    (fun br_qtree -> Briques.updated_tree br_qtree (x, y) (dx, dy))
    (Flux.constant br_qtree)

let rec update_etat : etat -> etat Flux.t =
  fun etat ->
  let palette, ball, score, (br_qtree, nb_br_touched) = etat in
  let score_flux = update_score score nb_br_touched in
  let palette_flux = update_palette () in
  let ball_flux = update_baballe palette_flux palette ball br_qtree in
  let briques_flux = update_briques br_qtree ball in
  (* modif flux *)
  let update_cond : etat -> bool =
    fun ((mouse_x, _, mouse_down), ((x, y), (dx, dy), is_launched), _, (br_qtree, _)) ->
    ((not is_launched) && mouse_down)
    || contact_x br_qtree (x, y) (dx, dy)
    || contact_y mouse_x br_qtree (x, y) (dx, dy)
  in
  let death_cond : etat -> bool =
    fun (_, ((_, y), (_, dy), _), _, _) -> y < -.BoxInit.marge && dy <= 0.0
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
