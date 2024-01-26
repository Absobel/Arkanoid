(* coordonnées de l'objet inséré *)
type coord = float * float

(* coordonnées des coins inférieur gauche et supérieur droit d'une case du quadtree *)
type bound = coord * coord

(* UTILS *)

(** [center_from_bounds] calcule le centre d'une case du quadtree à partir de ses coordonnées
    @param b les coordonnées des coins inférieur gauche et supérieur droit de la case
    @return le centre de la case *)
let center_from_bounds ((x1, y1), (x2, y2)) = (x1 +. x2) /. 2., (y1 +. y2) /. 2.

(* fonctions qui calculent les coordonnées des coins des quatre sous-cases d'une case du quadtree *)

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

(* QTREE *)

(** type du quadtree *)
type 'a t =
  | Empty of bound
  | Leaf of bound * coord * 'a
  | Node of bound * 'a t * 'a t * 'a t * 'a t

(** [empty] crée un quadtree vide
    @param b les coordonnées de la case initiale *)
let empty b = Empty b

(** [get] récupère la valeur associée à une coordonnée
    @param t le quadtree
    @param c la coordonnée
    @return la valeur associée à la coordonnée *)
let rec get : 'a t -> coord -> 'a option =
  fun t (x, y) ->
  match t with
  | Empty _ -> None
  | Leaf (_, _, v) -> Some v
  | Node (b, q1, q2, q3, q4) ->
    let cx, cy = center_from_bounds b in
    if x < cx
    then if y < cy then get q1 (x, y) else get q2 (x, y)
    else if y < cy
    then get q3 (x, y)
    else get q4 (x, y)

(** [insert] insère une valeur dans le quadtree
    @param t le quadtree
    @param c la coordonnée
    @param v la valeur à insérer
    @return le quadtree avec la valeur insérée *)
let rec insert : 'a t -> coord -> 'a -> 'a t =
  fun t c v ->
  match t with
  | Empty b -> Leaf (b, c, v)
  | Leaf (b, oc, ov) ->
    if c = oc
    then Leaf (b, c, v)
    else (
      let nt =
        Node (b, Empty (to_nw b), Empty (to_sw b), Empty (to_ne b), Empty (to_se b))
      in
      let nt = insert nt oc ov in
      insert nt c v)
  | Node (b, q1, q2, q3, q4) ->
    let x, y = c in
    let cx, cy = center_from_bounds b in
    (match x < cx, y < cy with
     | true, true -> Node (b, insert q1 c v, q2, q3, q4)
     | true, false -> Node (b, q1, insert q2 c v, q3, q4)
     | false, true -> Node (b, q1, q2, insert q3 c v, q4)
     | false, false -> Node (b, q1, q2, q3, insert q4 c v))

(** [prune_non_rec] supprime les sous-cases vides d'un noeud non récursivement
    @param t le quadtree
    @return le quadtree sans les sous-cases vides *)
let prune_non_rec t =
  match t with
  | Empty _ -> t
  | Leaf _ -> t
  | Node (b, q1, q2, q3, q4) as n ->
    (match q1, q2, q3, q4 with
     | Empty _, Empty _, Empty _, Empty _ -> Empty b
     | Leaf (_, c, v), Empty _, Empty _, Empty _ -> Leaf (b, c, v)
     | Empty _, Leaf (_, c, v), Empty _, Empty _ -> Leaf (b, c, v)
     | Empty _, Empty _, Leaf (_, c, v), Empty _ -> Leaf (b, c, v)
     | Empty _, Empty _, Empty _, Leaf (_, c, v) -> Leaf (b, c, v)
     | _ -> n)

(** [remove] supprime un objet du quadtree en élaguant si nécessaire
    @param t le quadtree
    @param c la coordonnée
    @return le quadtree sans l'objet *)
let rec remove t (x, y) =
  match t with
  | Empty _ -> t
  | Leaf (b, _, _) -> Empty b
  | Node (b, q1, q2, q3, q4) ->
    let cx, cy = center_from_bounds b in
    (match x < cx, y < cy with
     | true, true -> prune_non_rec (Node (b, remove q1 (x, y), q2, q3, q4))
     | true, false -> prune_non_rec (Node (b, q1, remove q2 (x, y), q3, q4))
     | false, true -> prune_non_rec (Node (b, q1, q2, remove q3 (x, y), q4))
     | false, false -> prune_non_rec (Node (b, q1, q2, q3, remove q4 (x, y))))

(** [iter_val] applique une fonction à tous les objets du quadtree
    @param t le quadtree
    @param f la fonction à appliquer *)
let rec iter_val t f =
  match t with
  | Empty _ -> ()
  | Leaf (_, _, v) -> f v
  | Node (_, q1, q2, q3, q4) ->
    iter_val q1 f;
    iter_val q2 f;
    iter_val q3 f;
    iter_val q4 f

(** [fold_val] filtre selon un prédicat et compte les objets enlevés
    @param t le quadtree
    @param f le prédicat
    @return
      le quadtree sans les objets qui ne vérifient pas le prédicat et le nombre d'objets enlevés *)
let filter_val_count_removal t f =
  let rec aux t acc =
    match t with
    | Empty _ -> t, acc
    | Leaf (b, _, v) -> if f v then t, acc else Empty b, acc + 1
    | Node (b, q1, q2, q3, q4) ->
      let q1, nacc = aux q1 acc in
      let q2, nacc = aux q2 nacc in
      let q3, nacc = aux q3 nacc in
      let q4, nacc = aux q4 nacc in
      prune_non_rec (Node (b, q1, q2, q3, q4)), nacc
  in
  aux t 0
