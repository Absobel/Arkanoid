open Libnewtonoid
open Iterator
open Game
open Init_values

(** [graphic_format] est le format de la fenetre *)
let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. BoxInit.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. BoxInit.marge) +. Box.supy -. Box.infy))

(** [draw_info] dessine les informations du jeu en bas à gauche de l'écran
    @param score le score du joueur
    @param lives le nombre de vies restantes *)
let draw_info (score, lives) =
  let score = string_of_int score in
  let lives = string_of_int lives in
  let info = "Score : " ^ score ^ " | Lives : " ^ lives in
  Graphics.set_color Graphics.black;
  (* Marche pas parce que personne s'est donné la peine de le faire marcher *)
  (* Graphics.set_text_size 20; *)
  Graphics.moveto 10 10;
  Graphics.draw_string info

(** [draw_ball] dessine la balle
    @param ball les coordonnées de la balle à dessiner (et autres infos non utilisées) *)
let draw_ball : ball -> unit =
  fun ((x, y), _, _) ->
  let x = int_of_float x in
  let y = int_of_float y in
  Graphics.set_color BallInit.color;
  Graphics.fill_circle x y (int_of_float BallInit.radius)

(** [draw_state] dessine l'état du jeu (appelle toutes les autres fonctions de dessin dans chaque module)
    @param etat l'état du jeu à dessiner *)
let draw_state : etat -> unit =
  fun etat ->
  let (mouse_x, _, _), ball, info, (br_tree, _) = etat in
  draw_info info;
  draw_ball ball;
  Palette.draw_palette mouse_x;
  Briques.draw_briques br_tree

(** [score] renvoie le score d'un état
    @param etat l'état dont on veut le score *)
let score etat : int =
  let _, _, (score, _), _ = etat in
  score

(** [draw] dessine l'état du jeu en boucle
    @param flux_etat le flux des états du jeu *)
let draw : etat Flux.t -> unit =
  fun flux_etat ->
  let rec loop : etat Flux.t -> int -> int =
    fun flux_etat last_score ->
    match Flux.(uncons flux_etat) with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      (* DESSIN ETAT *)
      draw_state etat;
      (* FIN DESSIN ETAT *)
      Graphics.synchronize ();
      Unix.sleepf PhysicsInit.dt;
      loop flux_etat' (last_score + score etat)
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let score = loop flux_etat 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()

(** [main] est la fonction principale
    on crée le flux avec l'était initial, un score initial et la valeur des vies initiales *)
let () = draw (Game.update_etat (Game.etat_init (0, OtherInit.init_lives)))

(* faire dune exec bin/newtonoid.exe pour run*)
