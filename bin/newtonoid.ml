(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game

module Init = struct
  let dt = 1000. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

(* etat : position (x,y), vitesse (dx,dy) et score *)
type etat = (float * float) * (float * float) * int

let draw_state (etat : etat) =
  match etat with
  | ((x, y), _, score) ->
    (* dessin de la balle *)
    Graphics.draw_circle (int_of_float x) (int_of_float y) 5;
    (* déplacement pour dessiner en bas à gauche de la fenetre*)
    Graphics.moveto 20 20;
    (* dessin du score *)
    Graphics.draw_string (string_of_int score)


(* extrait le score courant d'un etat : *)
let score etat : int = 
  match etat with
  | (_, _, score) -> score

let draw flux_etat =
  let rec loop flux_etat last_score =
    match Flux.(uncons flux_etat) with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      (* DESSIN ETAT *)
      draw_state etat;
      (* FIN DESSIN ETAT *)
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      loop flux_etat' (last_score + score etat)
    | _ -> assert false
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let score = loop flux_etat 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()

  
(* faire dune exec bin/newtonoid.exe pour run*)

let _ = failwith "TODO : modules Freefall / Bouncing / Collision / Mouse pour appel avec run"

(* 

(* position initiale de la balle au centre avec une vitesse nulle et un score égal à 0 *)
let _ = let init = ((400,300),(0,0),0) in
         draw (Freefall.run init)

*)


         

