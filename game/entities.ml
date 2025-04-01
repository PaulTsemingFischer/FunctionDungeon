open Root

let string_of_entity_status (e_status : GameEntity.status) =
  match e_status with
  | _ -> failwith "entity error: unsupported entity status"

let string_of_entity = GameEntity.string_of_entity string_of_entity_status

(* creating entities *)

let create_default_at e_type pos =
  match e_type with
  | Player -> GameEntity.create { health = 10.0 } Player [] pos
  | Wall -> GameEntity.create { health = 10.0 } Wall [] pos
  | Pigeon x -> GameEntity.create { health = 10.0 } (Pigeon x) [] pos

let entity_action_runner (state : GameState.t) (entity : GameEntity.t)
    (input : GameState.input) =
  match entity.entity_type with
  | Player -> Player.player_action state entity input
  | Pigeon _ -> Pigeon.pigeon_action state entity input
  | Wall -> state
