module Box = struct
  let height = 600.
  let width = 800.
  let marge = 10.
  let infx = 0. +. marge
  let infy = 0. +. marge
  let supx = width -. marge
  let supy = height -. marge
end

module Init = struct
  let g = 200.
  let dt = 1. /. 60. (* 60 Hz *)
  let vy_init = 500.

  (* impulse_facotr * (ball - centre de la palette) = facteur ajouté à la vitesse *)
  let impulse_factor = 8.0

  let etat =
    let palette = 0., false in
    let ball = (0., 0.), (0., vy_init), false in
    let score = 0 in
    let briques =
      failwith "TODO"
    in
    palette, ball, score, briques
end
