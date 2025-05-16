open GameDefinitions

val pigeon_action : GameState.t -> GameEntity.t -> 'a -> GameState.t
(** [pigeon_action state entity] returns a new game state from [state], after
    the pigeon [entity] has taken an action. *)
