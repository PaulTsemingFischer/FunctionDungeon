(*TYPES*)
(** [ground] represents a ground tile, does not block movement *)
type ground =
  | Void
  | Ground
  | Mud

  (** [weak_mob] represents a generic mob or pigeon *)
type weak_mob =
  | Variable_Range_and_Damage of int * float
  | Pigeon  

  (** [strong_mob] represents a mob with special effects *)
type strong_mob =
  | Small_Jailer
  | Medium_Jailer
  | Large_Jailer
  | Thief
  | Small_Fog
  | Large_Fog  (** generic mob or pigeon *)

    (** [item] item represents any obtainable item *)
type item =
  | ScaleAction of int
  | AddFire of float * int
  | AddDamage of float
  | AugmentToAdjacent
  | HealthItem of float

  (** [entity] represents a movement blocking entity(with the exception of Empty which does not block movement) *)
type entity =
  | Empty
  | Water
  | Lava
  | Fire
  | Wall
  | Door of int * Engine.Utils.vec2
  | Rock
  | WeakMob of weak_mob
  | StrongMob of strong_mob
  | Item of item
  | Player

  (** [tile] represents a square on the room grid *)
type tile = ground * entity
(** [t] represents a room of tiles *)
type t = tile array array
(** [world] represents a floor of rooms *)
type world = t list

(** [room_gen_settings] represents the modifiable settings used in room generation *)
type room_gen_settings = {
  gen_weak_mob : unit -> weak_mob;
  gen_strong_mob : unit -> strong_mob;
  gen_item : unit -> item;
  weak_mob_rate : float;
  strong_mob_rate : float;
  item_rate : float;
  room_width : int * int;
  room_height : int * int;
  min_room_coverage : float;
  island_liquify_chance : float;
  island_lava_chance : float;
  island_rock_chance : float;
  min_void_size : int;
  noise_room_wall_chance : float;
  rule_one_cave_merge_runs : int;
  rule_two_cave_merge_runs : int;
  num_rooms : int;
}

val default_room_gen_settings : room_gen_settings
(** [default_room_gen_settings] are good default parameters for room generation
*)

val string_of_genworld : t -> string
(**[string_of_genworld world] is a string representation of the proc gen world
   [world]*)

val generate_room : room_gen_settings -> t
(**[generate_room settings] is a randomly generated room with the provided
   settings*)

val generate_floor : room_gen_settings -> int * t list
(**[generate_floor settings] is a pair of the room id with the player and a
   randomly generated list of rooms with the provided settings*)

val to_tile_list : t -> (tile * Engine.Utils.vec2) list
(**[to_tile_list room] collects all tiles and their coords into a list*)
