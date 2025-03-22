type vec2 = int * int

let add_vec2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let neg_vec2 (x1, y1) = (-1 * x1, -1 * y1)
let sub_vec2 vec1 vec2 = add_vec2 vec1 (neg_vec2 vec2)

let string_of_vec vec =
  "(" ^ string_of_int (fst vec) ^ "," ^ string_of_int (snd vec) ^ ")"
