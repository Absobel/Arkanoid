open Libnewtonoid
open Iterator
open Game
open Init_values
open Briques

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. BoxInit.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. BoxInit.marge) +. Box.supy -. Box.infy))

let draw_score score =
  let score = string_of_int score in
  let score = "Score : " ^ score in
  Graphics.set_color Graphics.black;
  (* Marche pas parce que personne s'est donné la peine de le faire marcher *)
  (* Graphics.set_text_size 20; *)
  Graphics.moveto 10 10;
  Graphics.draw_string score

(* TODO : juste pour debug pour l'instant *)
let draw_state : etat -> unit =
  fun etat ->
  let (mouse_x, _), ball, score, br_tree = etat in
  draw_score score;
  Game.draw_ball ball;
  Palette.draw_palette (int_of_float mouse_x);
  Briques.draw_briques br_tree

(* extrait le score courant d'un etat : *)
let score etat : int =
  let _, _, score, _ = etat in
  score

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

let () = draw (Game.update_etat Init_values.etat_init)

(* faire dune exec bin/newtonoid.exe pour run*)
