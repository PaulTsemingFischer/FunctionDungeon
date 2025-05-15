open GameDefinitions
open GameState

val obstacle_action :
  GameState.t -> GameEntity.t -> Obstacles.obstacle -> 'a -> GameState.t
(** [obstacle_action state entity obstacle _] returns the updated game state
    after processing the action for an [obstacle] entity *)
