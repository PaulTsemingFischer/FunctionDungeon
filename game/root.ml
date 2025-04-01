open Engine

type game_stat = { health : float }

type entity_types =
  | Wall
  | Player
  | Pigeon of int

module BaseEntityDeclarations :
  Entity.EntityData with type t = game_stat and type entity_type = entity_types =
struct
  type t = game_stat
  type entity_type = entity_types

  let zeroed_stats = { health = 0.0 }
  let string_of_stats stat = Printf.sprintf "health: %f" stat.health

  let string_of_type e_type =
    match e_type with
    | Wall -> "wall"
    | Player -> "player"
    | Pigeon x -> Printf.sprintf "pigeon of %d" x
end

module GameEntity = Entity.Make (BaseEntityDeclarations)
module GameWorld = World.Make (GameEntity)
module GameState = State.Make (GameWorld)
