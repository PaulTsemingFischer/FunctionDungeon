open GameDefinitions
open Engine.Utils

type t

type input =
  | MovePlayer of vec2
  | Attack
  | Wait

exception Invalid_input of input

type event =
  | Move of GameEntity.t * vec2 * vec2
  | Say of GameEntity.t * string
  | ChangeHealth of GameEntity.t * float
  | ActivateActionModifier of
      GameEntity.t
      * Modifiers.possible_action list
      * Modifiers.possible_action list
  | ActivateMoveModifier of
      GameEntity.t * Modifiers.possible_move list * Modifiers.possible_move list
  | EntityDeath of GameEntity.t

and transition = t -> GameEntity.t -> input -> t

val string_of_event : event -> string

val room : t -> GameWorld.t
(**[room state] is the current room the player is in at the given state*)

val set_room: t -> GameWorld.t -> t
(**[set_room state room] is the state with the current room changed to [room]*)

val move_room: t -> int -> t
(**[move_room room_id] is the state with the current room in focus changed to the room specified by [room_id]*)

val create : GameWorld.t list -> ?tiles:GameTiles.t -> transition list -> t
(**[create rooms transitions] returns a gamestate with the specified rooms and
   transition list*)

val step : t -> input -> t
(**[step state input] takes an in-game turn and returns the updated state*)

val get_tiles : t -> GameTiles.t
(**[get_tiles state] returns a world of type [GameTiles.t] associated with
   [state]*)

val update_tiles : t -> GameTiles.t -> t
(**[update_tiles state tiles] returns an updated state whose tiles is [tiles]*)

val get_events : t -> (int * event) list
(**[get_events state] returns a list of tuples with each tuple being a turn and
   an event that occurred on that turn*)

val add_event : t -> event -> t
(**[add_event turn event] adds [event] to the stack of events in [t]*)

val get_turn : t -> int
(**[get_turn state] returns the turn of [state]*)

val get_player : t -> GameEntity.t
(**[get_player state] returns the player associated with [state]*)

val query_update_player : t -> t

val get_modifiers :
  t ->
  GameEntity.entity_type ->
  Modifiers.possible_actions_modifier list
  * Modifiers.possible_moves_modifier list
(**[get_modifiers state entity_type] returns a tuple of tile and movement
   modifiers associated with that entity type*)

val activate_action_modifiers :
  t ->
  entity_types ->
  Modifiers.possible_action list ->
  Modifiers.possible_action list
(**[activate_action_modifiers state entity_type possible_actions] applies all
   action modifiers associated with [entity_type] to [possible_actions] and
   returns the modified result (Does not change state in any way)*)

val activate_move_modifiers :
  t ->
  entity_types ->
  Modifiers.possible_move list ->
  Modifiers.possible_move list
(**[activate_move_modifiers state entity_type possible_moves] applies all move
   modifiers associated with [entity_type] to [possible_moves] and returns the
   modified result (Does not change state in any way)*)

val apply_action_modifiers :
  t ->
  GameEntity.t ->
  Modifiers.possible_action list ->
  t * Modifiers.possible_action list
(**[apply_action_modifiers state entity possible_actions] applies the action
   modifiers associated with [entity.entity_type] to [possible_actions],
   returning a list of new actions and the updated state, with the activation of
   the modifiers pushed to the event stack*)

val apply_move_modifiers :
  t ->
  GameEntity.t ->
  Modifiers.possible_move list ->
  t * Modifiers.possible_move list
(**[apply_move_modifiers state entity possible_moves] applies the move modifiers
   associated with [entity.entity_type] to [possible_moves], returning a list of
   new actions and the updated state, with the activation of the modifiers
   pushed to the event stack*)

val add_actions_modifier :
  t -> Modifiers.possible_actions_modifier -> entity_types -> t

val add_moves_modifier :
  t -> Modifiers.possible_moves_modifier -> entity_types -> t

val remove_actions_modifier : t -> entity_types -> t
val add_obstacle_to_world : t -> GameWorld.t -> vec2 -> Obstacles.obstacle -> t
val positions_in_radius : vec2 -> int -> vec2 list
val build_barrier : t -> GameWorld.t -> vec2 -> int -> Obstacles.obstacle -> t
val remove_actions_modifier : t -> entity_types -> t
val add_obstacle_to_world : t -> GameWorld.t -> vec2 -> Obstacles.obstacle -> t
val positions_in_radius : vec2 -> int -> vec2 list
val build_barrier : t -> GameWorld.t -> vec2 -> int -> Obstacles.obstacle -> t
