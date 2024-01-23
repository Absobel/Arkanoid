open Iterator

(* OBJECTS *)

(* pos * velocity * is_launched *)
type ball = (float * float) * (float * float) * bool

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

module Init = struct
  let g = 200.
  let dt = 1. /. 60. (* 60 Hz *)
  let vy_init = 500.

  (* impulse_facotr * (ball - centre de la palette) = facteur ajouté à la vitesse *)
  let impulse_factor = 8.0

  let etat =
    let palette = 0., false in
    let ball : ball = (0., 0.), (0., vy_init), false in
    let score = 0 in
    let briques =
      let create_brick x y =
        let random_color =
          Graphics.rgb (Random.int 256) (Random.int 256) (Random.int 256)
        in
        (x *. 100.0, y *. 50.0, (x +. 1.0) *. 100.0, (y +. 1.0) *. 50.0, random_color)
      in
      let rec create_row x y =
        if x >= 7.0 then [] else create_brick x y :: create_row (x +. 1.0) y
      in
      let rec create_briques x y =
        if y >= 15.0 then [] else create_row x y @ create_briques x (y +. 1.0)
      in
      create_briques 3.0 8.0
    in
    palette, ball, score, briques
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

module Brique = struct
  (* x1, y1, x2, y2, color *)
  type t = float * float * float * float * Graphics.color

  let edge_coord b =
    let x1, y1, x2, y2, _ = b in
    x1, y1, x2, y2

  let color b =
    let _, _, _, _, c = b in
    c

  let contact_y_one_brick br (bx, by) dy =
    let x1, y1, x2, y2 = edge_coord br in
    let contact_up =
      by -. float_of_int Ball.radius <= y2
      && by >= y1
      && dy <= 0.0
      && bx >= x1
      && bx <= x2
    in
    let contact_down =
      by +. float_of_int Ball.radius >= y1
      && by <= y2
      && dy >= 0.0
      && bx >= x1
      && bx <= x2
    in
    contact_down || contact_up

  let contact_x_one_brick br (bx, by) dx =
    let x1, y1, x2, y2 = edge_coord br in
    let contact_left =
      bx +. float_of_int Ball.radius >= x1
      && bx <= x2
      && dx >= 0.0
      && by >= y1
      && by <= y2
    in
    let contact_right =
      bx -. float_of_int Ball.radius <= x2
      && bx >= x1
      && dx <= 0.0
      && by >= y1
      && by <= y2
    in
    contact_left || contact_right

  let contact_one_brick br (bx, by) dx dy =
    contact_x_one_brick br (bx, by) dx || contact_y_one_brick br (bx, by) dy

  let contact_x br_list bx dx =
    List.fold_left
      (fun acc br -> acc || contact_x_one_brick br bx dx)
      false
      br_list

  let contact_y br_list by dy =
    List.fold_left
      (fun acc br -> acc || contact_y_one_brick br by dy)
      false
      br_list

  let updated_list br_list (bx, by) (dx, dy) =
    List.filter (fun br -> not (contact_one_brick br (bx, by) dx dy)) br_list

  let draw_brique b =
    let x1, y1, x2, y2, c = b in
    let x1, y1, x2, y2 =
      int_of_float x1, int_of_float y1, int_of_float x2, int_of_float y2
    in
    Graphics.set_color c;
    Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1)

  let draw_briques br_list =
    List.iter draw_brique br_list
end

type brique = Brique.t
type palette = float * bool
type etat = palette * ball * int * brique list

(* DRAWING FUNCTIONS *)

let draw_ball : ball -> unit =
  fun ((x, y), _, _) ->
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

let contact_x br_list (x, y) dx =
  (x > Box.supx && dx >= 0.0)
  || (x < Box.infx && dx <= 0.0)
  || Brique.contact_x br_list (x, y) dx

let contact_high_y y dy = y > Box.supy && dy >= 0.0
let contact_low_y y dy = y < -.Box.marge && dy <= 0.0

let contact_y mouse_x br_list (x, y) dy =
  contact_high_y y dy
  || Palette.contact mouse_x (x, y) dy
  || Brique.contact_y br_list (x, y) dy

let rebond_x br_list x dx = if contact_x br_list x dx then -.dx else dx

let rebond_y br_list mouse_x (x, y) dy =
  if contact_y mouse_x br_list (x, y) dy then -.dy else dy

(* La balle devient toute seule de plus en plus rapide due à la gravité *)
let rec update_etat : etat -> etat Flux.t =
  fun etat ->
  let (mouse_x, mouse_down), ((x, y), (dx, dy), is_launched), score, br_list = etat in
  (* score *)
  let score_flux = Flux.unfold (fun s -> Some (s, s + 1)) score in
  (* palette *)
  let palette_flux =
    Flux.unfold
      (fun () ->
        let x, _ = Graphics.mouse_pos () in
        Some ((float_of_int x, Graphics.button_down ()), ()))
      ()
  in
  (* baballe *)
  let new_is_launched = is_launched || mouse_down in
  let ball_flux =
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
      let ndx = rebond_x br_list (x, y) dx +. impulse in
      let ndy = rebond_y br_list mouse_x (x, y) dy in
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
  in
  (* briques *)
  let brique_flux =
    Flux.map
      (fun br_list -> Brique.updated_list br_list (x, y) (dx, dy))
      (Flux.constant br_list)
  in
  (* modif flux *)
  let update_cond : etat -> bool =
    fun ((mouse_x, mouse_down), ((x, y), (dx, dy), is_launched), _, br_list) ->
    ((not is_launched) && mouse_down)
    || contact_x br_list (x, y) dx
    || contact_y mouse_x br_list (x, y) dy
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
          brique_flux)
       update_cond
       update_etat)
    death_cond
    (fun _ -> Flux.vide)
