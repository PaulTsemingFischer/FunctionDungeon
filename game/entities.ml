open Root

(* creating entities *)

let create_default_at e_type pos : GameWorld.e_t =
  match e_type with
  | Player -> GameEntity.create { health = 10.0 } Player [] pos
  | Wall -> GameEntity.create { health = 10.0 } Wall [] pos
  | Pigeon x -> GameEntity.create { health = 10.0 } (Pigeon x) [] pos

let entity_action_runner (state : GameState.t) (entity : GameEntity.t)
    (input : input) =
  match entity.entity_type with
  | Player -> Player.player_action state entity input
  | Pigeon _ -> Pigeon.pigeon_action state entity input
  | Wall -> state
