(** Valeurs de réglage modifiables *)

(** valeurs initiales de la fenêtre de jeu*)
module BoxInit = struct
  let height = 600.
  let width = 800.
  let marge = 10.
end

(** valeurs initiales de la physique *)
module PhysicsInit = struct
  let g = 200.
  let dt = 1. /. 60. (* 60 Hz *)

  (* impulse_facotr * vitesse_palette = facteur ajouté à la vitesse *)
  let impulse_factor = 0.3
end

(** valeurs initiales de la balle *)
module BallInit = struct
  let radius = 10.0
  let color = Graphics.rgb 255 0 0
  let vy_init = 500.
end

(** valeurs initiales des briques *)
module BriquesInit = struct
  let br_height = 50.
  let br_width = 100.
  let score_per_br = 100

  (* liste de briques de type : ((float*float) * Graphics.color) list *)
  (* Les briques peuvent être placées que sur une grille de briques, les coords vont snap sur la coord de grille la plus proche *)
  let br_list =
    let create_brick x y =
      (x, y), Graphics.rgb (Random.int 255) (Random.int 255) (Random.int 255)
    in
    let rec fill acc x y max_x max_y =
      if x > max_x
      then fill acc 0. (y +. br_height) max_x max_y
      else if y > max_y
      then acc
      else fill (create_brick x y :: acc) (x +. br_width) y max_x max_y
    in
    fill [] 0. (br_height *. 4.) BoxInit.width BoxInit.height
end

(** valeurs initiales de la palette *)
module PaletteInit = struct
  let width = 100.0
  let height = 10.0
  let color = Graphics.rgb 0 0 0
  let pos_y = 20.0
end

(** valeurs initiales diverses sur l'état du jeu *)
module OtherInit = struct
  let init_lives = 3
end
