open Engine
open Engine.Utils

type game_stat = { health : float }

module BaseGameStat : Entity.StatType with type t = game_stat = struct
  type t = game_stat

  let zeroed_stats = { health = 0.0 }
  let string_of_stats stat = Printf.sprintf "health: %f" stat.health
end

module GameEntity = Entity.Make (BaseGameStat)
module GameWorld = World.Make (GameEntity)
module GameState = State.Make (GameWorld)

type GameState.input += Move of vec2
type GameEntity.entity_type += Wall | Player
type GameEntity.rendering += Ascii of char | Id_debug

let string_of_game_types (e_type : GameEntity.entity_type) =
  match e_type with
  | Wall -> "\"wall\""
  | Player -> "\"Player\""
  | _ -> failwith "game error: unsupported entity type"

let string_of_game_rendering (e_rendering : GameEntity.rendering) =
  match e_rendering with
  | Ascii ch -> String.make 1 ch
  | Id_debug -> "\"id_renderer\""
  | _ -> failwith "game error: unsupported rendering type"

let string_of_game_status (e_status : GameEntity.status) =
  match e_status with
  | _ -> failwith "game error: unsupported entity status"

let string_of_game_entity =
  GameEntity.string_of_entity string_of_game_types string_of_game_rendering
    string_of_game_status

let create_wall_at = GameEntity.create { health = 0. } Wall (Ascii '#') []

let print_all_entities world =
  List.iter
    (fun ent -> print_endline (string_of_game_entity ent))
    (GameWorld.all_entities world)

let player_action_generator (state : GameState.t) (entity : GameEntity.t) input
    =
  match input with
  | Move dir ->
      if not (GameWorld.query_empty state.world (add_vec2 entity.pos dir)) then
        raise (GameState.Invalid_input input)
      else
        Some
          ( 0,
            fun state : GameState.t ->
              GameState.update_world state
                (GameWorld.put_entity state.world
                   (GameEntity.set_pos entity (add_vec2 dir entity.pos))) )
  | _ -> None

let entity_action_generator =
  GameState.Generator
    (fun (state : GameState.t)
      (entity : GameEntity.t)
      (input : GameState.input)
    ->
      match entity.entity_type with
      | Player -> player_action_generator state entity input
      | Wall -> None
      | _ ->
          print_endline "unsupported_entity";
          None)

let rec print_world_region (world : GameWorld.t) ((x1, y1) : int * int)
    ((x2, y2) : int * int) =
  if y1 > y2 then ()
  else
    let rec print_row (world : GameWorld.t) ((xr, yr) : int * int) (end_x : int)
        =
      if xr > end_x then ()
      else (
        (match GameWorld.query_pos world (xr, yr) with
        | None -> print_string (String.make 1 '.' ^ " ")
        | Some ent -> (
            match ent.rendering with
            | Ascii x -> print_string (String.make 1 x ^ " ")
            | Id_debug -> Printf.printf "%-2s" (GameEntity.string_of_id ent.id)
            | _ -> failwith "game error: unsupported renderer"));
        print_row world (xr + 1, yr) end_x)
    in
    print_row world (x1, y2) x2;
    print_newline ();
    print_world_region world (x1, y1) (x2, y2 - 1)

let rec loop (state : GameState.t) =
  ignore (Sys.command "clear");
  print_world_region state.world (-10, -10) (10, 10);
  print_endline ("Turn number " ^ string_of_int state.turn);
  print_string "wasd to move, q to quit: ";
  let input = String.trim (read_line ()) in
  try
    match input with
    | "w" -> loop (GameState.step state (Move (0, 1)))
    | "a" -> loop (GameState.step state (Move (-1, 0)))
    | "s" -> loop (GameState.step state (Move (0, -1)))
    | "d" -> loop (GameState.step state (Move (1, 0)))
    | "q" -> ()
    | _ -> loop state
  with GameState.Invalid_input input_val -> (
    match input_val with
    | Move vec -> loop state
    | _ -> print_endline "unknown invalid action")

let () =
  let player = GameEntity.create { health = 1. } Player (Ascii '@') [] (0, 0) in
  let world = GameWorld.put_entity GameWorld.empty player in
  let world_with_walls =
    List.fold_left
      (fun (current_world : GameWorld.t) wall_pos ->
        GameWorld.put_entity current_world (create_wall_at wall_pos))
      world
      (List.append
         (List.append
            (List.init 10 (fun x -> (-5 + x, 5)))
            (List.init 10 (fun x -> (-5 + x, -5))))
         (List.append
            (List.init 11 (fun x -> (5, -5 + x)))
            (List.init 10 (fun x -> (-5, x - 5)))))
  in
  let state = GameState.create world_with_walls [ entity_action_generator ] in
  loop state
