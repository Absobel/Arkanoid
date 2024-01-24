open Game

type bound = (float * float) * (float * float)

(* UTILS *)

let center_from_bounds ((x1, y1), (x2, y2)) = (x1 +. x2) /. 2., (y1 +. y2) /. 2.

let to_nw b =
  let (x1, y1), (_, _) = b in
  let cx, cy = center_from_bounds b in
  (x1, y1), (cx, cy)

let to_ne b =
  let (_, y1), (x2, _) = b in
  let cx, cy = center_from_bounds b in
  (cx, y1), (x2, cy)

let to_sw b =
  let (x1, _), (_, y2) = b in
  let cx, cy = center_from_bounds b in
  (x1, cy), (cx, y2)

let to_se b =
  let (_, _), (x2, y2) = b in
  let cx, cy = center_from_bounds b in
  (cx, cy), (x2, y2)

(* BOX *)

(* QTREE *)

type 'a qtree =
  | Empty
  | Leaf of 'a
  | Node of bound * 'a qtree * 'a qtree * 'a qtree * 'a qtree

let empty = Empty

let rec get t (x, y) =
  match t with
  | Empty -> None
  | Leaf v -> Some v
  | Node ((cx, cy), q1, q2, q3, q4) ->
    if x < cx
    then if y < cy then get q1 (x, y) else get q2 (x, y)
    else if y < cy
    then get q3 (x, y)
    else get q4 (x, y)

let insert t (x, y) v =
  let rec insert_aux t v center =
    match t with
    | Empty -> Leaf v
    | Leaf ov ->
      let nt = Node (center, Empty, Empty, Empty, Empty) in
      let nt = insert_aux nt ov center in
      insert_aux nt v center
    | Node (b, q1, q2, q3, q4) ->
      let cx, cy = center_from_bounds b in
      (match x < cx, y < cy with
       | true, true -> Node (b, insert_aux q1 v (to_nw b), q2, q3, q4)
       | true, false -> Node (b, q1, insert_aux q2 v (to_sw b), q3, q4)
       | false, true -> Node (b, q1, q2, insert_aux q3 v (to_ne b), q4)
       | false, false -> Node (b, q1, q2, q3, insert_aux q4 v (to_se b)))
  in
  insert_aux t v ((Box.infx, Box.infy), (Box.supx, Box.supy))

  let prune_non_rec t = 
    match t with
    | Empty -> Empty
    | Leaf _ -> t
    | Node (_, q1, q2, q3, q4) as n ->
        match q1, q2, q3, q4 with
        | Empty, Empty, Empty, Empty -> Empty
        | Leaf v, Empty, Empty, Empty -> Leaf v
        | Empty, Leaf v, Empty, Empty -> Leaf v
        | Empty, Empty, Leaf v, Empty -> Leaf v
        | Empty, Empty, Empty, Leaf v -> Leaf v
        | _ -> n

let rec remove t (x, y) =
  match t with
  | Empty -> Empty
  | Leaf _ -> Empty
  | Node (b, q1, q2, q3, q4) ->
    let cx, cy = center_from_bounds b in
    (match x < cx, y < cy with
     | true, true -> prune_non_rec (Node (b, remove q1 (x, y), q2, q3, q4))
     | true, false -> prune_non_rec (Node (b, q1, remove q2 (x, y), q3, q4))
     | false, true -> prune_non_rec (Node (b, q1, q2, remove q3 (x, y), q4))
     | false, false -> prune_non_rec (Node (b, q1, q2, q3, remove q4 (x, y))))