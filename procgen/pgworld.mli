(*TYPES*)
type ground =
  | Void
  | Ground
  | Mud

type weak_mob =
  | PlaceHolderWeakMob
  | Pigeon

type strong_mob = PlaceHolderStrongMob
type item = PlaceHolderItem

type entity =
  | Empty
  | Water
  | Lava
  | Fire
  | Wall
  | Door of t
  | Rock
  | WeakMob of weak_mob
  | StrongMob of strong_mob
  | Item of item

and t = tile array array
and tile = ground * entity

val default_entity : tile

type room_gen_settings = {
  gen_weak_mob : unit -> weak_mob;
  gen_strong_mob : unit -> strong_mob;
  weak_mob_rate : float;
  strong_mob_rate : float;
  item_rate : float;
  room_width : int;
  room_height : int;
  min_room_coverage : float;
  island_liquify_chance : float;
  island_lava_chance : float;
  island_rock_chance : float;
  min_void_size : int;
  num_doors : int;
  noise_room_wall_chance : float;
  rule_one_cave_merge_runs : int;
  rule_two_cave_merge_runs : int;
}

val default_room_gen_settings : room_gen_settings

val string_of_genworld : t -> string
(**[string_of_genworld world] is a string representation of the proc gen world
   [world]*)

val generate_room : room_gen_settings -> t
(**[generate_room settings] is a randomly generated room with the provided
   settings*)

val to_tile_list : t -> (tile * Engine.Utils.vec2) list
(**[to_tile_list room] collects all tiles and their coords into a list*)
