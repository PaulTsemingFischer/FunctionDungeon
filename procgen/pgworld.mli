(*TYPES*)
type ground =
  | Void
  | Ground
  | Mud

type weak_mob =
  | PlaceHolderWeakMob
  | Pigeon

type strong_mob =
  | Jailer
  | Thief
  | Blinder

  type item =
  | ScaleAction of int
  | AddFire of float * int
  | AddDamage of float
  | AugmentToAdjacent

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

type tile = ground * entity
type t = tile array array
type world = t list

val default_entity : tile

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
