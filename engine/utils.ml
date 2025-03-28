type vec2 = int * int

(*Translation methods*)
let add_vec2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let neg_vec2 (x1, y1) = (-1 * x1, -1 * y1)
let sub_vec2 vec1 vec2 = add_vec2 vec1 (neg_vec2 vec2)

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

let string_of_vec vec =
  "(" ^ string_of_int (fst vec) ^ "," ^ string_of_int (snd vec) ^ ")"
