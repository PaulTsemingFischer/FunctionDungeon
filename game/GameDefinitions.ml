open Engine
open Engine.Utils

type entity_stat = {
  health : float;
  base_actions : Modifiers.possible_action list;
  base_moves : Modifiers.possible_move list;
}

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

type status_effects = Fire of float * int

let string_of_type e_type =
  match e_type with
  | Wall -> "wall"
  | Player -> "player"
  | Pigeon -> "pigeon"
  | Door (i, _) -> "door" ^ string_of_int i
  | Rock -> "rock"
  | Fire -> "fire"
  | Water -> "water"
  | Lava -> "lava"
  | HorizontalBouncer _ -> "h-bouncer"
  | Enemy e -> Enemytype.string_of_enemy e
  | Obstacle o -> Obstacles.string_of_obstacle o
  | ModifierItem m -> Modifiers.string_of_modifier m
  | SpecialItem -> "special-item"

(** [is_killable_entity entity_type] is true if [entity_type] can take
    damage/die, otherwise false. *)
let is_killable_entity e_type =
  match e_type with
  | Player | Pigeon | HorizontalBouncer _ | Enemy _ -> true
  | _ -> false

module BaseEntityDeclarations :
  Entity.EntityData
    with type t = entity_stat
     and type entity_type = entity_types
     and type status_effect = status_effects = struct
  type t = entity_stat
  type entity_type = entity_types
  type status_effect = status_effects

  let string_of_stats stat = Printf.sprintf "health: %f" stat.health
  let string_of_type = string_of_type
  let string_of_type = string_of_type
  let string_of_status (_ : status_effect) = "generic"
end

module GameEntity = Entity.Make (BaseEntityDeclarations)

(**[create_default_at entity_type pos] returns an entity of type [entity_type]
   with its position set to [pos]*)
let create_default_at e_type pos : GameEntity.t =
  Modifiers.(
    match e_type with
    | Player ->
        GameEntity.create
          {
            health = 10.0;
            base_moves = base_cross_moves;
            base_actions = base_cross_actions;
          }
          Player [] pos
    | Wall ->
        GameEntity.create
          { health = 10.0; base_moves = []; base_actions = [] }
          Wall [] pos
    | Rock ->
        GameEntity.create
          { health = 10.0; base_moves = []; base_actions = [] }
          Rock [] pos
    | Water ->
        GameEntity.create
          { health = 10.0; base_moves = []; base_actions = [] }
          Water [] pos
    | Fire ->
        GameEntity.create
          { health = 10.0; base_moves = []; base_actions = [] }
          Fire [] pos
    | Lava ->
        GameEntity.create
          { health = 10.0; base_moves = []; base_actions = [] }
          Lava [] pos
    | Pigeon ->
        GameEntity.create
          {
            health = 3.0;
            base_moves = base_cross_moves;
            base_actions = base_cross_actions;
          }
          Pigeon [] pos
    | Door (i, dir) ->
        GameEntity.create
          { health = 10.0; base_moves = []; base_actions = [] }
          (Door (i, dir))
          [] pos
    | HorizontalBouncer x ->
        GameEntity.create
          { health = 10.0; base_moves = []; base_actions = [] }
          (HorizontalBouncer x) [] pos
    | Enemy e ->
        GameEntity.create
          {
            health = 10.0;
            base_moves = base_cross_moves;
            base_actions = enemy_cross_actions e;
          }
          (Enemy e) [] pos
    | Obstacle o ->
        GameEntity.create
          { health = 10.0; base_moves = []; base_actions = [] }
          (Obstacle o) [] pos
    | ModifierItem m ->
        GameEntity.create
          { health = 10.0; base_moves = []; base_actions = [] }
          (ModifierItem m) [] pos
    | SpecialItem ->
        GameEntity.create
          { health = 10.0; base_moves = []; base_actions = [] }
          SpecialItem [] pos)

(**[print_entities entity_list] prints all entities in given entity_list*)
let print_entities entity_list =
  List.iter (fun x -> print_endline (GameEntity.string_of_entity x)) entity_list

module GameWorld = World.Make (GameEntity)

type tile_stat = unit

type tile_types =
  | Ground
  | Mud

let string_of_tile_type t_type =
  match t_type with
  | Ground -> "ground"
  | Mud -> "mud"

module BaseTileDeclarations :
  Entity.EntityData
    with type t = tile_stat
     and type entity_type = tile_types
     and type status_effect = status_effects = struct
  type t = tile_stat
  type entity_type = tile_types
  type status_effect = status_effects

  let string_of_stats stat = Printf.sprintf "tile: no stats"
  let string_of_type = string_of_tile_type
  let string_of_status (_ : status_effect) = "generic"
end

module TileEntity = Entity.Make (BaseTileDeclarations)

(**[create_tile_at tile_type pos] returns an entity of type [tile_type] with its
   position set to [pos]*)
let create_tile_at t_type pos : TileEntity.t =
  Modifiers.(TileEntity.create () t_type [] pos)

(**[print_tiles tile_list] prints all entities in given [tile_list]*)
let print_tiles tile_list =
  List.iter (fun x -> print_endline (TileEntity.string_of_entity x)) tile_list

module GameTiles = World.Make (TileEntity)
