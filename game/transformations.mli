open GameDefinitions
open Modifiers
open Engine.Utils
open Procgen

(**[apply_move state entity possible_move] moves an entity to [possible_move]
   relative to its current position. If the entity is not in the world when this
   is run, or the location is not empty, then this transformation will return an
   unchanged state*)

val apply_move : GameState.t -> GameWorld.e_t -> vec2 -> GameState.t

val apply_pickup_move : GameState.t -> GameWorld.e_t -> vec2 -> GameState.t
(** [apply_pickup_move state entity possible_move] picks up the item at the
    target position, adding it to the list of action modifiers for [entity], and
    then calls [apply_move] with the updated state. If the target position has
    no item, behaves identically to [apply_move state entity possible_move]. *)

val say : GameState.t -> GameWorld.e_t -> string -> GameState.t
(**[say priority state entity message] makes an entity say something (cosmetic
   effect for events)*)

exception Entity_not_found of GameEntity.t

val apply_action_to : GameState.t -> GameWorld.e_t -> action -> GameState.t
(**[apply_action_to state entity action] applies [action] to [entity], returning
   an updated [state] with the changed entity*)

val apply_actions_to : GameState.t -> GameWorld.e_t -> action list -> GameState.t
(**[apply_action_to state entity actions] applies several [actions] to [entity], returning
   an updated [state] with the changed entity*)

val normal_room : GameWorld.e_t -> Pgworld.t -> GameWorld.t * GameTiles.t
(**[normal_room state player] is a new entity world, tile world pair with the
   given player*)

val generate_floor :
  GameWorld.e_t ->
  Pgworld.room_gen_settings ->
  (GameState.t -> GameWorld.e_t -> GameState.input -> GameState.t) list ->
  GameState.t
(**[generate_world player settings] is a floor with the given [settings] and
   [player] *)

val apply_attack_to_entity :
  GameState.t -> GameWorld.e_t -> action list -> GameState.t
(** [apply_attack_to_entity] applies a single list of actions onto [entity] and
    returns the updated game state. *)

val apply_attack_to : GameState.t -> vec2 -> possible_action list -> GameState.t
(** [apply_attack_to state actions] applies all actions in [actions] to the game
    state [state]. Since attack coordinates are relative to player position, the
    value of [player_pos] determines the actual tiles affected. Attacks cannot
    hit the player itself; that is, any action on tile (0,0) will be skipped. *)
