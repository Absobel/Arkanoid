(* pos * velocity * is_launched *)
module Ball = struct
  type t = (float * float) * (float * float) * bool

  let radius = 10
  let color = Graphics.rgb 255 0 0
end