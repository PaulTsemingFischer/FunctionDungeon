open GameDefinitions

val player_action :
  GameState.t -> GameWorld.e_t -> GameState.input -> GameState.t
(** [player_action state entity input] returns a new game state from [state],
    after the player [entity] has taken the action specified by [input]. *)
