open Game

module type Rect = sig
  type t = float * float * float * float

  val is_inside : t -> float * float -> bool
end

module type QuadtreeRect = sig
  module R : Rect

  type t

  val empty : t
  val get : t -> float * float -> R.t option
  val insert : t -> R.t -> t
  val remove : t -> R.t -> t
end

module Quadtree (R : Rect) : QuadtreeRect = struct
  module R = R

  type center = float * float

  type cases = t * t * t * t

  and t =
    | Empty
    | Leaf of center * R.t
    | Node of center * cases

  let empty = Empty

  let rec get t (x, y) =
    match t with
    | Empty -> None
    | Leaf (_, b) -> if R.is_inside b (x, y) then Some b else None
    | Node ((cx, cy), (nw, ne, sw, se)) ->
      (match x < cx, y < cy with
       | true, true -> get nw (x, y)
       | true, false -> get sw (x, y)
       | false, true -> get ne (x, y)
       | false, false -> get se (x, y))

  let insert t b = failwith "TODO"
  let remove t b = failwith "TODO"
end
