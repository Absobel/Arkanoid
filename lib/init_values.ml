(* INIT VALUES *)

module BoxInit = struct
  let height = 600.
  let width = 800.
  let marge = 10.
end

module PhysicsInit = struct
  let g = 200.
  let dt = 1. /. 60. (* 60 Hz *)
  let vy_init = 500.

  (* impulse_facotr * (ball - centre de la palette) = facteur ajouté à la vitesse *)
  let impulse_factor = 8.0
end

module BallInit = struct
  let radius = 10
  let color = Graphics.rgb 255 0 0
end

module BriquesInit = struct
  let br_height = 50.
  let br_width = 100.

  (* Les briques peuvent être placées que sur une grille de briques, les coords vont snap sur la coord de grille la plus proche *)
  let br_list =
    [ (250., 250.), Graphics.rgb (Random.int 255) (Random.int 255) (Random.int 255)
    ; (300., 300.), Graphics.rgb (Random.int 255) (Random.int 255) (Random.int 255)
    ]
end

module PaletteInit = struct
  let width = 100
  let height = 10
  let color = Graphics.rgb 0 0 0
  let pos_y = 20
end
