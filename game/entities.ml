open Root

let entity_action_runner (state : GameState.t) (entity : GameEntity.t)
    (input : input) =
  match entity.entity_type with
  | Player -> Player.player_action state entity input
  | Pigeon -> Pigeon.pigeon_action state entity input
  | Wall -> state
