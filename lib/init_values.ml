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
end

module PaletteInit = struct
  let width = 100
  let height = 10
  let color = Graphics.rgb 0 0 0
  let pos_y = 20
end

let etat_init =
  let palette = 0., false in
  let ball = (0., 0.), (0., PhysicsInit.vy_init), false in
  let score = 0 in
  let briques =
    Quadtree.insert
      (Quadtree.insert
         (Quadtree.empty ((0., 0.), (BoxInit.width, BoxInit.height)))
         (200., 200.)
         ((200., 200.), Graphics.red))
      (200., 300.)
      ((300., 300.), Graphics.green)
  in
  palette, ball, score, briques
