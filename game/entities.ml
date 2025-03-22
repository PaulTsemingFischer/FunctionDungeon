open Root

type GameEntity.entity_type += Wall | Player
type GameEntity.rendering += Ascii of char | Id_debug

(* printing entities *)
let string_of_entity_types (e_type : GameEntity.entity_type) =
  match e_type with
  | Wall -> "wall"
  | Player -> "player"
  | _ -> failwith "entity error: unsupported entity type"

let string_of_entity_rendering (e_rendering : GameEntity.rendering) =
  match e_rendering with
  | Ascii ch -> String.make 1 ch
  | Id_debug -> "\"id_renderer\""
  | _ -> failwith "entity error: unsupported rendering type"

let string_of_entity_status (e_status : GameEntity.status) =
  match e_status with
  | _ -> failwith "entity error: unsupported entity status"

let string_of_game_entity =
  GameEntity.string_of_entity string_of_entity_types string_of_entity_rendering
    string_of_entity_status

(* creating entities *)

let create_default_entity_at e_type pos =
  match e_type with
  | Player -> GameEntity.create { health = 10.0 } Player (Ascii '@') [] pos
  | Wall -> GameEntity.create { health = 10.0 } Wall (Ascii '#') [] pos
  | _ -> failwith "entity error: create unsupported entity type"

let entity_action_generator =
  GameState.Generator
    (fun (state : GameState.t)
      (entity : GameEntity.t)
      (input : GameState.input)
    ->
      match entity.entity_type with
      | Player -> Player.player_action_generator state entity input
      | Wall -> None
      | _ ->
          print_endline "unsupported_entity";
          None)
