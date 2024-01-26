open Iterator
open Init_values

(* OBJECTS *)

(** module contenant des valeurs utiles en rapport avec la fenêtre de jeu, pas assez pour mériter son propre fichier *)
module Box = struct
  open Init_values.BoxInit

  let infx = 0. +. marge
  let infy = 0. +. marge
  let supx = width -. marge
  let supy = height -. marge
end

(** position * vitesse * est_lancée *)
type ball = (float * float) * (float * float) * bool
(** position souris (= position palette) * vitesse * est_souris_cliquée *)
type palette = float * float * bool
(** score * vies *)
type info = int * int
(** état du jeu *)
type etat = palette * ball * info * (Briques.t * int)

(* UTILS *)

(** [etat_init] permet de définir l'état initial du jeu
  @param info score et vies initiaux
  @return état initial du jeu *)
let etat_init info =
  let palette = 0., 0., false in
  let ball = (0., 0.), (0., BallInit.vy_init), false in
  let score = info in
  let briques = Briques.br_list_to_qtree BriquesInit.br_list, 0 in
  palette, ball, score, briques

(** [integre] fonction qui intègre/somme les valeurs successives du flux avec 
              un pas de temps dt et une valeur initiale nulle, i.e. 
              acc_0 = 0; acc_{i+1} = acc_{i} + dt * flux_{i}
  @param dt pas de temps
  @param flux flux à intégrer
  @return flux intégré *)
let integre dt flux =
  (* valeur initiale de l'intégrateur *)
  let init = 0., 0. in
  (* fonction auxiliaire de calcul de acc_{i} + dt * flux_{i} *)
  let iter (acc1, acc2) (flux1, flux2) = acc1 +. (dt *. flux1), acc2 +. (dt *. flux2) in
  (* définition récursive du flux acc *)
  let rec acc = Tick (lazy (Some (init, Flux.map2 iter acc flux))) in
  acc

(** [contact_x] teste le contact avec une surface verticale
    (bords de la fenêtre ou briques)
  @param br_qtree arbre des briques
  @param (x, y) position de la balle
  @param (dx, dy) vitesse de la balle
  @return true si la balle est en contact avec une surface verticale *)
let contact_x br_qtree (x, y) (dx, dy) =
  (x > Box.supx && dx >= 0.0)
  || (x < Box.infx && dx <= 0.0)
  || fst (Briques.contact br_qtree (x, y) (dx, dy))

(** [contact_y] teste le contact avec une surface horizontale
    (bord supérieur de la fenêtre, palette ou briques)
  @param mouse_x position de la souris (= position de la palette)
  @param br_qtree arbre des briques
  @param (x, y) position de la balle
  @param (dx, dy) vitesse de la balle
  @return true si la balle est en contact avec une surface horizontale *)
let contact_y mouse_x br_qtree (x, y) (dx, dy) =
  (y > Box.supy && dy >= 0.0)
  || Palette.contact mouse_x (x, y) dy
  || snd (Briques.contact br_qtree (x, y) (dx, dy))

(** [rebond_x] change la vitesse si rebond sur une surface horizontale
  @param br_qtree arbre des briques
  @param (x, y) position de la balle
  @param (dx, dy) vitesse de la balle
  @return nouvelle vitesse de la balle après un éventuel rebond horizontal *)
let rebond_x br_qtree p (dx, dy) = if contact_x br_qtree p (dx, dy) then -.dx else dx

(** [rebond_y] change la vitesse si rebond sur une surface verticale
  @param mouse_x position de la souris (= position de la palette)
  @param br_qtree arbre des briques
  @param (x, y) position de la balle
  @param (dx, dy) vitesse de la balle
  @return nouvelle vitesse de la baller après un éventuel rebond vertical *)
let rebond_y br_qtree mouse_x p (dx, dy) =
  if contact_y mouse_x br_qtree p (dx, dy) then -.dy else dy

(* GAME LOGIC *)

(** [update_score] crée le flux de score par rapport au score précédent et au nombre de briques touchées
  @param (score, lives) score et vies actuels (vies non modifiées)
  @param nb_br_touched nombre de briques touchées par la balle depuis la dernière mise à jour
  @return flux d'informations sur le jeu *)
let update_info (score, lives) nb_br_touched =
  Flux.constant (score + (nb_br_touched * BriquesInit.score_per_br), lives)

(** [update_palette] crée le flux de palette (position, vitesse et s'il y a clic gauche)
  @return flux de palette *)
let update_palette () =
  Flux.unfold
    (fun prev_x ->
      let x, _ = Graphics.mouse_pos () in
      let x = float_of_int x in
      let dx = (x -. prev_x) /. PhysicsInit.dt in
      Some ((x, dx, Graphics.button_down ()), x))
    0.0

(** [update_baballe] crée le flux de balle (position, vitesse et si elle est lancée)
  @param palette_flux flux de palette
  @param palette palette
  @param ball balle
  @param br_qtree arbre des briques
  @return flux de balle *)
let update_baballe : palette flux -> palette -> ball -> Briques.t -> ball Flux.t =
  fun palette_flux
    (mouse_x, mouse_dx, mouse_down)
    ((x, y), (dx, dy), is_launched)
    br_qtree ->
  (* passe à vrai une seule fois et le reste : quand le premier clic est réalisé *)
  let new_is_launched = is_launched || mouse_down in
  if new_is_launched
  (* jeu normal *)
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
  else (* la balle reste au niveau de la palette jusqu'au premier clic ou elle est envoyée
          a une vitesse y intiale et une vitesse x qui est la vitesse de la palette *)
    Flux.map2
      (fun (mouse_x, mouse_dx, _) dy ->
        ( (mouse_x, PaletteInit.pos_y +. (BallInit.radius /. 2.))
        , (mouse_dx, dy)
        , new_is_launched ))
      palette_flux
      (Flux.constant dy)

(** [update_briques] crée le flux de briques (arbre des briques et nombre de briques touchées depuis la dernière mise à jour)
  @param br_qtree arbre des briques
  @param ball balle
  @return flux de briques *)
let update_briques : Briques.t -> ball -> (Briques.t * int) Flux.t =
  fun br_qtree ((x, y), (dx, dy), _) ->
  Flux.map
    (fun br_qtree -> Briques.updated_tree br_qtree (x, y) (dx, dy))
    (Flux.constant br_qtree)

(** [update_etat] crée le flux d'état du jeu (palette, balle, score, briques)
  @param etat état du jeu
  @return flux d'état du jeu *)
let rec update_etat : etat -> etat Flux.t =
  fun etat ->
  let palette, ball, (score, lives), (br_qtree, nb_br_touched) = etat in
  let score_flux = update_info (score, lives) nb_br_touched in
  let palette_flux = update_palette () in
  let ball_flux = update_baballe palette_flux palette ball br_qtree in
  let briques_flux = update_briques br_qtree ball in
  (* check s'il y a le premier clic ou s'il y a contact avec quoique ce soit sauf le bord inférieur de la fenêtre *)
  let update_cond : etat -> bool =
    fun ((mouse_x, _, mouse_down), ((x, y), (dx, dy), is_launched), _, (br_qtree, _)) ->
    ((not is_launched) && mouse_down)
    || contact_x br_qtree (x, y) (dx, dy)
    || contact_y mouse_x br_qtree (x, y) (dx, dy)
  in
  (* check s'il y a contact (mortel) avec le bord inférieur de la fenêtre *)
  let death_cond : etat -> bool =
    fun (_, ((_, y), (_, dy), _), _, _) -> y < -.BoxInit.marge && dy <= 0.0
  in
  (* flux si rien ne change *)
  let flux_continue =
    Flux.map4 (fun p b s br -> p, b, s, br) palette_flux ball_flux score_flux briques_flux
  in
  (* flux si le joueur est mauvais (termine la partie s'il n'y a plus de vies) *)
  let flux_death _ =
    if lives == 1 then Flux.vide else update_etat (etat_init (score, lives - 1))
  in
  (* le flux se met à jour à chaque contact ou se réinitialise en cas de mort *)  
  Flux.unless (Flux.unless flux_continue update_cond update_etat) death_cond flux_death
