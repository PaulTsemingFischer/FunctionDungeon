(*Randomization*)
let random_element list =
  let index = Random.int (List.length list) in
  List.nth list index

type vec2 = int * int

(*Translation methods*)
let add_vec2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let neg_vec2 (x1, y1) = (-1 * x1, -1 * y1)
let sub_vec2 vec1 vec2 = add_vec2 vec1 (neg_vec2 vec2)
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

let get_at_vec (arr : 'a matrix) (row, col) = arr.(row).(col)

let get_at_vec_opt (arr : 'a matrix) (row, col) =
  try Some arr.(row).(col) with Invalid_argument _ -> None

let set_at_vec (arr : 'a matrix) (row, col) x = arr.(row).(col) <- x

(*Misc*)
let neighbors vec =
  [
    add_vec2 vec (0, 1);
    add_vec2 vec (1, 0);
    add_vec2 vec (0, -1);
    add_vec2 vec (-1, 0);
  ]

let random_dir () =
  match Random.int 4 with
  | 0 -> (1, 0)
  | 1 -> (0, 1)
  | 2 -> (-1, 0)
  | _ -> (0, -1)
