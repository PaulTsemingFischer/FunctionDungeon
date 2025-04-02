open Engine
open Engine.Utils

type game_stat = {
  health : float;
  base_actions : Modifiers.possible_action list;
  base_moves : Modifiers.possible_move list;
}

type entity_types =
  | Wall
  | Player
  | Pigeon of int

type status_effects = Fire of int

let string_of_type e_type =
  match e_type with
  | Wall -> "wall"
  | Player -> "player"
  | Pigeon _ -> "pigeon"

module BaseEntityDeclarations :
  Entity.EntityData
    with type t = game_stat
     and type entity_type = entity_types
     and type status_effect = status_effects = struct
  type t = game_stat
  type entity_type = entity_types
  type status_effect = status_effects

  let zeroed_stats =
    {
      health = 0.0;
      base_moves = Modifiers.base_cross_moves;
      base_actions = Modifiers.base_cross_actions;
    }

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
    | Pigeon x ->
        GameEntity.create
          {
            health = 10.0;
            base_moves = base_cross_moves;
            base_actions = base_cross_actions;
          }
          (Pigeon x) [] pos)

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
  (**[create world transitions] returns a gamestate with the specified world and
     transition list*)

  val step : t -> input -> t
  (**[step state input] takes an in-game turn and returns the updated state*)

  val get_world : t -> GameWorld.t
  (**[get_world state] returns a world of type [GameWorld.t] associated with
     [state]*)

  val update_world : t -> GameWorld.t -> t
  (**[update_world state world] returns an updated state whose world is [world]*)

  val get_events : t -> (int * event) list
  (**[get_events state] returns a list of tuples with each tuple being a turn
     and an event that occurred on that turn*)

  val add_event : t -> event -> t
  (**[add_event turn event] adds [event] to the stack of events in [t]*)

  val get_turn : t -> int
  (**[get_turn state] returns the turn of [state]*)

  val get_player : t -> GameEntity.t
  (**[get_player state] returns the player associated with [state]*)

  val get_modifiers :
    t ->
    GameEntity.entity_type ->
    Modifiers.possible_actions_modifier list
    * Modifiers.possible_moves_modifier list
  (**[get_modifiers state entity_type] returns a tuple of tile and movement
     modifiers associated with that entity type*)

  val add_actions_modifier :
    t -> Modifiers.possible_actions_modifier -> entity_types -> t

  val add_moves_modifier :
    t -> Modifiers.possible_moves_modifier -> entity_types -> t
end

module GameState : GameStateSignature = struct
  type t = {
    world : GameWorld.t;
    transitions : transition list;
    events : (int * event) list;
    turn : int;
    player : GameWorld.e_t;
    modifiers :
      (string
      * (Modifiers.possible_actions_modifier list
        * Modifiers.possible_moves_modifier list))
      list;
  }

  and transition = t -> GameEntity.t -> input -> t

  let create (world : GameWorld.t) (transitions : transition list) : t =
    {
      world;
      transitions;
      events = [];
      turn = 0;
      player =
        GameEntity.create
          {
            health = 10.0;
            base_moves = Modifiers.base_cross_moves;
            base_actions = Modifiers.base_cross_actions;
          }
          Player [] (0, 0);
      modifiers = [];
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
      modifiers = new_state.modifiers;
    }

  let get_world state = state.world

  let update_world { world; transitions; events; turn; player; modifiers }
      new_world : t =
    { world = new_world; transitions; events; turn; player; modifiers }

  let get_events state = state.events

  let add_event { world; transitions; events; turn; player; modifiers } event =
    {
      world;
      transitions;
      events = (turn, event) :: events;
      turn;
      player;
      modifiers;
    }

  let get_turn state = state.turn
  let get_player state = state.player

  let get_modifiers state entity_type :
      Modifiers.possible_actions_modifier list
      * Modifiers.possible_moves_modifier list =
    match List.assoc_opt (string_of_type entity_type) state.modifiers with
    | None -> ([], [])
    | Some x -> x

  let add_actions_modifier state possible_action entity_type =
    match List.assoc_opt (string_of_type entity_type) state.modifiers with
    | None ->
        {
          world = state.world;
          transitions = state.transitions;
          events = state.events;
          player = state.player;
          turn = state.turn;
          modifiers =
            (string_of_type entity_type, ([ possible_action ], []))
            :: state.modifiers;
        }
    | Some (possible_action_list, movement_modifier_list) ->
        let removed_modifier_assoc =
          List.remove_assoc (string_of_type entity_type) state.modifiers
        in
        {
          world = state.world;
          transitions = state.transitions;
          events = state.events;
          player = state.player;
          turn = state.turn;
          modifiers =
            ( string_of_type entity_type,
              (possible_action :: possible_action_list, movement_modifier_list)
            )
            :: removed_modifier_assoc;
        }

  let add_moves_modifier state movement_modifier entity_type =
    match List.assoc_opt (string_of_type entity_type) state.modifiers with
    | None ->
        {
          world = state.world;
          transitions = state.transitions;
          events = state.events;
          player = state.player;
          turn = state.turn;
          modifiers =
            (string_of_type entity_type, ([], [ movement_modifier ]))
            :: state.modifiers;
        }
    | Some (possible_action_list, movement_modifier_list) ->
        let removed_modifier_assoc =
          List.remove_assoc (string_of_type entity_type) state.modifiers
        in
        {
          world = state.world;
          transitions = state.transitions;
          events = state.events;
          player = state.player;
          turn = state.turn;
          modifiers =
            ( string_of_type entity_type,
              (possible_action_list, movement_modifier :: movement_modifier_list)
            )
            :: removed_modifier_assoc;
        }
end
