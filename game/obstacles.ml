open Root

type obstacle = Spreading_Fire of int * int * int
(* fire patch is currently centered at c with radius r and growing at g rate *)

let string_of_obstacle (o : obstacle) =
  match o with
  | Spreading_Fire (c, r, g) -> "spreading fire"
