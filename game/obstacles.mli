open Engine.Utils

exception WrongObsType

type obstacle =
  | Spreading_Fire of vec2 * int * int
  (* fire patch is currently centered at c with radius r and growing at g
     rate *)
  | Fence of int (* fence t exists on the game board for t turns *)

val string_of_obstacle : obstacle -> string
(** [string_of_obstacle] is the string representation of each obstacle type *)

val get_c : obstacle -> vec2
(** [get_c obstacle] is the center position of a spreading fire obstacle
    @raise WrongObsType if called on a non-spreading fire obstacle *)

val get_r : obstacle -> int
(** [get_r obstacle] is the radius of a spreading fire obstacle
    @raise WrongObsType if called on a non-spreading fire obstacle *)

val get_g : obstacle -> int
(** [get_g obstacle] is the growth rate of a spreading fire obstacle
    @raise WrongObsType if called on a non-spreading fire obstacle *)

val update_obstacle_age : obstacle -> obstacle
(** [update_obstacle_age obstacle] decreases the age of a fence obstacle by one
    @raise WrongObsType if called on a non-fence obstacle *)

val grow_fire : obstacle -> obstacle
(** [grow_fire obstacle] increases the radius of a spreading fire obstacle by
    its growth rate
    @raise WrongObsType if called on a non-spreading fire obstacle *)
