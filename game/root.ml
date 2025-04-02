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

  let string_of_event event =
    match event with
    | Move (entity, startpos, endpos) ->
        Printf.sprintf "Entity %s moved from %s to %s"
          (GameEntity.string_of_id entity.id)
          (string_of_vec2 startpos) (string_of_vec2 endpos)
    | Say (entity, message) ->
        Printf.sprintf "Entity %s says: %s"
          (GameEntity.string_of_id entity.id)
          message

  type t = {
    world : GameWorld.t;
    transitions : transition list;
    events : event list;
    turn : int;
    player : GameWorld.e_t;
  }

  and transition = t -> GameEntity.t -> input -> t

  let create (world : GameWorld.t) (transitions : transition list) : t =
    {
      world;
      transitions;
      events = [];
      turn = 0;
      player = GameEntity.create { health = 10.0 } Player [] (0, 0);
    }

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
    let updated_player =
      List.find_opt
        (fun (e : GameEntity.t) -> e.entity_type = Player)
        (GameWorld.all_entities new_state.world)
    in
    {
      world = new_state.world;
      transitions = new_state.transitions;
      events = new_state.events;
      turn = new_state.turn + 1;
      player =
        (match updated_player with
        | Some p -> p
        | None -> state.player);
    }

  let update_world { world; transitions; events; turn; player } new_world : t =
    { world = new_world; transitions; events; turn; player }
end
