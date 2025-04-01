open Root

(* how to add an entity:

   1. add entity to the entity_type variant with +=

   2. add entity to the string_of_entity_types function

   3. add entity to the create_default_at

   4. define entity behavior using transformations in entity_action_generator *)

type GameEntity.rendering += Ascii of char | Id_debug

let string_of_entity_rendering (e_rendering : GameEntity.rendering) =
  match e_rendering with
  | Ascii ch -> String.make 1 ch
  | Id_debug -> "id_renderer"
  | _ -> failwith "entity error: unsupported rendering type"

let string_of_entity_status (e_status : GameEntity.status) =
  match e_status with
  | _ -> failwith "entity error: unsupported entity status"

let string_of_entity =
  GameEntity.string_of_entity string_of_entity_rendering string_of_entity_status

(* creating entities *)

let create_default_at e_type pos =
  match e_type with
  | Player -> GameEntity.create { health = 10.0 } Player (Ascii '@') [] pos
  | Wall -> GameEntity.create { health = 10.0 } Wall (Ascii '#') [] pos
  | Pigeon x ->
      GameEntity.create { health = 10.0 } (Pigeon x) (Ascii 'p') [] pos

let entity_action_runner (state : GameState.t) (entity : GameEntity.t)
    (input : GameState.input) =
  match entity.entity_type with
  | Player -> Player.player_action state entity input
  | Pigeon _ -> Pigeon.pigeon_action state entity input
  | Wall -> state
