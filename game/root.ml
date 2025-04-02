open Engine
open Engine.Utils

type game_stat = { health : float }

type entity_types =
  | Wall
  | Player
  | Pigeon of int

type status_effects = Fire of int

let string_of_type e_type =
  match e_type with
  | Wall -> "wall"
  | Player -> "player"
  | Pigeon x -> Printf.sprintf "pigeon of %d" x

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
  let string_of_type = string_of_type

  let string_of_type e_type =
    match e_type with
    | Wall -> "wall"
    | Player -> "player"
    | Pigeon x -> Printf.sprintf "pigeon of %d" x

  let string_of_status (_ : status_effect) = "generic"
end

module GameEntity = Entity.Make (BaseEntityDeclarations)
module GameWorld = World.Make (GameEntity)

type input =
  | MovePlayer of vec2
  | Wait

exception Invalid_input of input

type event =
  | Move of GameEntity.t * vec2 * vec2
  | Say of GameEntity.t * string

let string_of_event event =
  match event with
  | Move (entity, startpos, endpos) ->
      Printf.sprintf "%s moved from %s to %s"
        (string_of_type entity.entity_type)
        (string_of_vec2 startpos) (string_of_vec2 endpos)
  | Say (entity, message) ->
      Printf.sprintf "%s says: %s" (string_of_type entity.entity_type) message

module type GameStateSignature = sig
  type t
  and transition = t -> GameEntity.t -> input -> t

  val create : GameWorld.t -> transition list -> t
  (**[create world transitions] returns a gamestate with the specified world and transition list*)
  val step : t -> input -> t
  (**[step state input] takes an in-game turn and returns the updated state*)

  val get_world : t -> GameWorld.t
  (**[get_world state] returns a world of type [GameWorld.t] associated with [state]*)

  val update_world : t -> GameWorld.t -> t
  (**[update_world state world] returns an updated state whose world is [world]*)

  val get_events : t -> (int * event )list
  (**[get_events state] returns a list of tuples with each tuple being a turn and an event that occurred on that turn*)

  val add_event : t -> event -> t
  (**[add_event turn event] adds [event] to the stack of events in [t]*)

  val get_turn : t -> int
  (**[get_turn state] returns the turn of [state]*)

  val get_player : t -> GameEntity.t
  (**[get_player state] returns the player associated with [state]*)
end

module GameState:GameStateSignature = struct
  type t = {
    world : GameWorld.t;
    transitions : transition list;
    events : (int*event) list;
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

  let get_world state = state.world
  let update_world { world; transitions; events; turn; player } new_world : t =
    { world = new_world; transitions; events; turn; player }

  let get_events state = state.events

  let add_event {world;transitions;events;turn;player} event = {world;transitions;events = (turn, event) :: events; turn; player}

  let get_turn state = state.turn

  let get_player state = state.player
  
end
