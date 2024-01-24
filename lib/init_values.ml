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
    let briquess =
      let create_brick x y =
        let random_color =
          Graphics.rgb (Random.int 256) (Random.int 256) (Random.int 256)
        in
        x *. 100.0, y *. 50.0, (x +. 1.0) *. 100.0, (y +. 1.0) *. 50.0, random_color
      in
      let rec create_row x y =
        if x >= 7.0 then [] else create_brick x y :: create_row (x +. 1.0) y
      in
      let rec create_briquess x y =
        if y >= 15.0 then [] else create_row x y @ create_briquess x (y +. 1.0)
      in
      create_briquess 3.0 8.0
    in
    palette, ball, score, briquess
end
