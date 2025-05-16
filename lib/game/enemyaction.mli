open GameDefinitions
open GameState
open Engine.Utils

val enemy_action :
  GameState.t -> GameEntity.t -> Enemytype.enemy -> 'a -> GameState.t
(** [enemy_action] is the updated state after an [entity] of a certain enemy
    type [enemy] takes an action *)
