(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator
open Input

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

(* TODO : juste pour debug pour l'instant *)
let draw_state : etat -> unit =
  fun etat ->
  let palette, ball, score = etat in
  Game.draw_palette palette;
  Game.draw_ball ball

(* extrait le score courant d'un etat : *)
let score etat : int =
  (* match etat with
     | (_, _, score) -> score *)
  0

let draw : etat Flux.t -> unit =
  fun flux_etat ->
  let rec loop : etat Flux.t -> score -> score =
    fun flux_etat last_score ->
    match Flux.(uncons flux_etat) with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      (* DESSIN ETAT *)
      draw_state etat;
      (* FIN DESSIN ETAT *)
      Graphics.synchronize ();
      Unix.sleepf (Init.dt);
      loop flux_etat' (last_score + score etat)
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let score = loop flux_etat 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()

let () = draw (Game.update_etat Init.etat)

(* faire dune exec bin/newtonoid.exe pour run*)

(* let _ = failwith "TODO : modules Freefall / Bouncing / Collision / Mouse pour appel avec run" *)

(*
   (* position initiale de la balle au centre avec une vitesse nulle et un score égal à 0 *)
   let _ = let init = ((400,300),(0,0),0) in
   draw (Freefall.run init)
*)
