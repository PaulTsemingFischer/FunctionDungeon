open Engine.Utils

type obstacle =
  | Spreading_Fire of int * int * int
  (* fire patch is currently centered at c with radius r and growing at g
     rate *)
  | Fence

(** [string_of_obstacle] is the string representation of each obstacle type *)
let string_of_obstacle (o : obstacle) =
  match o with
  | Spreading_Fire (c, r, g) -> "spreading fire"
  | Fence -> "fence"
