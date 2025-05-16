open Engine
open Engine.Utils

type entity_stat = {
  health : float;
  base_actions : Modifiers.possible_action list;
  base_moves : Modifiers.possible_move list;
}
(** [entity_stat] represents the stats of a game entity. *)

(** [entity_types] represents all types of entities that can spawn in a game
    world. *)
type entity_types =
  | Wall
  | Player
  | Pigeon
  | Door of int * vec2
    (* Index in room arr and the spawn location the player should be in after
       going through the door*)
  | Rock
  | Fire
  | Water
  | Lava
  | HorizontalBouncer of bool
  | Enemy of Enemytype.enemy
  | Obstacle of Obstacles.obstacle
  | ModifierItem of Modifiers.possible_actions_modifier
  | SpecialItem
  | HealthItem of float

(** [status_effects] represents all possible status effects that can be on an
    entity. Fire (x,y) is a status effect that deals x damage each turn for y
    turns. *)
type status_effects = Fire of float * int

val string_of_type : entity_types -> string
(** [string_of_type entity_type] is the string representation of [entity_type].
*)

val is_killable_entity : entity_types -> bool
(** [is_killable_entity entity_type] is true if [entity_type] can take
    damage/die, otherwise false. *)

module BaseEntityDeclarations : Entity.EntityData
module GameEntity : Entity.S

val create_default_at : entity_types -> vec2 -> GameEntity.t
(**[create_default_at entity_type pos] returns an entity of type [entity_type]
   with its position set to [pos]*)

val print_entities : GameEntity.t list -> unit
(**[print_entities entity_list] prints all entities in given entity_list*)

module GameWorld : World.S

type tile_stat = unit

type tile_types =
  | Ground
  | Mud

val string_of_tile_type : tile_types -> string
(** [string_of_tine_type tile_type] is the string representation of [tile_type].
*)

module BaseTileDeclarations : Entity.EntityData
module TileEntity : Entity.S

val create_tile_at : tile_types -> vec2 -> TileEntity.t
(**[create_tile_at tile_type pos] returns an entity of type [tile_type] with its
   position set to [pos]*)

val print_tiles : TileEntity.t list -> unit
(**[print_tiles tile_list] prints all entities in given [tile_list]*)

module GameTiles : World.S

val to_entity_types : GameEntity.entity_type -> entity_types
val to_tile_types : TileEntity.entity_type -> tile_types
val to_entity_type : entity_types -> GameEntity.entity_type
val to_entity_stats : GameEntity.stats -> entity_stat
val to_entity_id : int -> GameWorld.e_id
val to_gameworld_type : GameEntity.t -> GameWorld.e_t
val to_gameentity_type : GameWorld.e_t -> GameEntity.t
val to_gameentity_stats : entity_stat -> GameEntity.stats
val to_status_effect : status_effects -> GameEntity.status_effect
val to_status_effects : GameEntity.status_effect -> status_effects
val to_gametiles_type : TileEntity.t -> GameTiles.e_t
val to_tileentity_type : GameTiles.e_t -> TileEntity.t
