open Engine

type game_stat = {
  health : float;
  base_actions : Modifiers.possible_action list;
  base_moves : Modifiers.possible_move list;
}

type entity_types =
  | Wall
  | Player
  | Pigeon
  | Door

type status_effects = Fire of int

let string_of_type e_type =
  match e_type with
  | Wall -> "wall"
  | Player -> "player"
  | Pigeon -> "pigeon"
  | Door -> "door"

module BaseEntityDeclarations :
  Entity.EntityData
    with type t = game_stat
     and type entity_type = entity_types
     and type status_effect = status_effects = struct
  type t = game_stat
  type entity_type = entity_types
  type status_effect = status_effects

  let string_of_stats stat = Printf.sprintf "health: %f" stat.health
  let string_of_type = string_of_type

  let string_of_type e_type =
    match e_type with
    | Wall -> "wall"
    | Player -> "player"
    | Pigeon -> "pigeon"
    | Door -> "door"

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
    | Pigeon ->
        GameEntity.create
          {
            health = 3.0;
            base_moves = base_cross_moves;
            base_actions = base_cross_actions;
          }
          Pigeon [] pos
    | Door ->
        GameEntity.create
          { health = 10.0; base_moves = []; base_actions = [] }
          Door [] pos)

(**[print_entities entity_list] prints all entities in given entity_list*)
let print_entities entity_list =
  List.iter (fun x -> print_endline (GameEntity.string_of_entity x)) entity_list

module GameWorld = World.Make (GameEntity)
