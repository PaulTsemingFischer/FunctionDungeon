open Engine
open Engine.Utils

type game_stat = { health : float }

type entity_types =
  | Wall
  | Player
  | Pigeon of int

type status_effects = Fire of int

module BaseEntityDeclarations :
  Entity.EntityData
    with type t = game_stat
     and type entity_type = entity_types
     and type status_effect = status_effects = struct
  type t = game_stat
  type entity_type = entity_types
  type status_effect = status_effects

  let zeroed_stats = { health = 0.0 }
  let string_of_stats stat = Printf.sprintf "health: %f" stat.health

  let string_of_type e_type =
    match e_type with
    | Wall -> "wall"
    | Player -> "player"
    | Pigeon x -> Printf.sprintf "pigeon of %d" x

  let string_of_status (_ : status_effect) = "generic"
end

module GameEntity = Entity.Make (BaseEntityDeclarations)
module GameWorld = World.Make (GameEntity)

module GameState = struct
  type input =
    | MovePlayer of vec2
    | Wait

  type event =
    | Move of GameEntity.t * vec2 * vec2
    | Say of GameEntity.t * string

  type t = {
    world : GameWorld.t;
    transitions : transition list;
    events : event list;
    turn : int;
  }

  and transition = t -> GameEntity.t -> input -> t

  let create (world : GameWorld.t) (transitions : transition list) : t =
    { world; transitions; events = []; turn = 0 }

  exception Invalid_input of input

  let step (state : t) (input : input) =
    let new_state =
      List.fold_left
        (fun (state_ext : t) (entity : GameEntity.t) ->
          List.fold_left
            (fun (acc : t) (transition : transition) ->
              transition state_ext entity input)
            state_ext state_ext.transitions)
        state
        (GameWorld.all_entities state.world)
    in
    {
      world = new_state.world;
      transitions = new_state.transitions;
      events = new_state.events;
      turn = new_state.turn + 1;
    }

  let update_world { world; transitions; events; turn } new_world : t =
    { world = new_world; transitions; events; turn }
end
