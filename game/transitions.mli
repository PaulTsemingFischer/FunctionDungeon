open GameDefinitions

val entity_status_runner :
  GameState.t -> GameWorld.e_t -> GameState.input -> GameState.t
(** [entity_status_runner state entity input] applies the status effects
    associated with [entity] on the given [state]. *)

val entity_action_runner :
  GameState.t -> GameWorld.e_t -> GameState.input -> GameState.t
(**[entity_action_runner state entity input] executes the actions associated
   with some entity on the given [state]*)
