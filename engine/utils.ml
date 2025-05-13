(*Randomization*)
let random_element list =
  let index = Random.int (List.length list) in
  List.nth list index

type vec2 = int * int

type cardinal_dir =
  | N
  | E
  | S
  | W

(*Translation methods*)
let add_vec2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let neg_vec2 (x1, y1) = (-1 * x1, -1 * y1)
let sub_vec2 vec1 vec2 = add_vec2 vec1 (neg_vec2 vec2)
let scale_vec2 (x, y) s = (x * s, y * s)
let mul_vec2 (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
let scale_vec2 (x, y) s = (x * s, y * s)
let mul_vec2 (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

let string_of_vec2 vec =
  "(" ^ string_of_int (fst vec) ^ "," ^ string_of_int (snd vec) ^ ")"

type vec2f = float * float

let add_vec2f (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
let neg_vec2f (x1, y1) = (-1. *. x1, -1. *. y1)
let sub_vec2f vec1 vec2 = add_vec2f vec1 (neg_vec2f vec2)
let scale_vec2f (x, y) s = (x *. s, y *. s)
let mul_vec2f (x1, y1) (x2, y2) = (x1 *. x2, y1 *. y2)
let round_vec2f (x, y) = (Float.round x, Float.round y)
let vec2_of_vec2f (x, y) = (int_of_float x, int_of_float y)
let vec2f_of_vec2 (x, y) = (float_of_int x, float_of_int y)

let string_of_vec2f vec =
  "(" ^ string_of_float (fst vec) ^ "," ^ string_of_float (snd vec) ^ ")"

let lerp f1 f2 t = f1 +. ((f2 -. f1) *. t)
let lerp_vec (v1x, v1y) (v2x, v2y) t = (lerp v1x v2x t, lerp v1y v2y t)
let length_squared vf = (fst vf *. fst vf) +. (snd vf *. snd vf)

(*2d array access*)
type 'a matrix = 'a array array

let get_at_vec (arr : 'a matrix) (row, col) =
  try arr.(row).(col)
  with Invalid_argument msg ->
    failwith
      ("Invalid row: " ^ string_of_int row ^ " col: " ^ string_of_int col
     ^ " in get_at_vec call in Engine.utils.ml\n" ^ msg)

let get_at_vec_opt (arr : 'a matrix) (row, col) =
  try Some arr.(row).(col) with Invalid_argument _ -> None

let set_at_vec (arr : 'a matrix) (row, col) x =
  try arr.(row).(col) <- x
  with Invalid_argument msg ->
    failwith
      ("Invalid row: " ^ string_of_int row ^ " col: " ^ string_of_int col
     ^ " in set_at_vec call in Engine.utils.ml\n" ^ msg)

let apply_at_vecs (arr : 'a matrix) spots f =
  List.iter (fun (row, col) -> arr.(row).(col) <- f row col) spots

let dimensions (arr : 'a matrix) =
  let rows = Array.length arr in
  let cols = if rows > 0 then Array.length arr.(0) else 0 in
  (rows, cols)

(*Misc*)
let cardinal_neighbors vec =
  [
    add_vec2 vec (0, 1);
    add_vec2 vec (1, 0);
    add_vec2 vec (0, -1);
    add_vec2 vec (-1, 0);
  ]

let cardinal_neighbors_with_dir vec =
  [
    (add_vec2 vec (0, 1), S);
    (add_vec2 vec (1, 0), E);
    (add_vec2 vec (0, -1), N);
    (add_vec2 vec (-1, 0), W);
  ]

let random_cardinal_dir () =
  match Random.int 4 with
  | 0 -> (1, 0)
  | 1 -> (0, 1)
  | 2 -> (-1, 0)
  | _ -> (0, -1)

let principal_neighbors vec =
  [
    add_vec2 vec (0, 1);
    add_vec2 vec (1, 1);
    add_vec2 vec (1, 0);
    add_vec2 vec (1, -1);
    add_vec2 vec (0, -1);
    add_vec2 vec (-1, -1);
    add_vec2 vec (-1, 0);
    add_vec2 vec (-1, 1);
  ]

let principal_neighbors_gen2 vec =
  [
    add_vec2 vec (0, 2);
    add_vec2 vec (1, 2);
    add_vec2 vec (2, 2);
    add_vec2 vec (2, 1);
    add_vec2 vec (2, 0);
    add_vec2 vec (2, -1);
    add_vec2 vec (2, -2);
    add_vec2 vec (1, -2);
    add_vec2 vec (0, -2);
    add_vec2 vec (-1, -2);
    add_vec2 vec (-2, -2);
    add_vec2 vec (-2, -1);
    add_vec2 vec (-2, 0);
    add_vec2 vec (-2, 1);
    add_vec2 vec (-2, 2);
    add_vec2 vec (-1, 2);
  ]

let random_principal_dir () =
  match Random.int 8 with
  | 0 -> (0, 1)
  | 1 -> (1, 1)
  | 2 -> (1, 0)
  | 3 -> (1, -1)
  | 4 -> (0, -1)
  | 5 -> (-1, -1)
  | 6 -> (-1, 0)
  | _ -> (-1, 1)

let opposite = function
  | N -> S
  | E -> W
  | S -> N
  | W -> E
