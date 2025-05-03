open Engine.Utils


exception WrongObsType

type obstacle =
  | Spreading_Fire of vec2 * int * int
  (* fire patch is currently centered at c with radius r and growing at g
     rate *)
  | Fence of int (* fence t exists on the game board for t turns *)

(** [string_of_obstacle] is the string representation of each obstacle type *)
let string_of_obstacle (o : obstacle) =
  match o with
  | Spreading_Fire (c, r, g) -> "spreading fire"
  | Fence t -> "fence"

let get_c obstacle =
  match obstacle with
  | Fence t -> raise WrongObsType
  | Spreading_Fire (c, r, g) -> c

let get_r obstacle =
  match obstacle with
  | Fence t -> raise WrongObsType
  | Spreading_Fire (c, r, g) -> r

let get_g obstacle =
  match obstacle with
  | Fence t -> raise WrongObsType
  | Spreading_Fire (c, r, g) -> g

let update_obstacle_age obstacle =
  match obstacle with
  | Fence t -> Fence (t - 1)
  | Spreading_Fire (c, r, g) -> raise WrongObsType

let grow_fire obstacle =
  match obstacle with
  match obstacle with
  | Fence t -> raise WrongObsType
  | Spreading_Fire (c, r, g) -> Spreading_Fire (c, r + g, g)
