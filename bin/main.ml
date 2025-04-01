open Engine
open Engine.Utils
open Game.Root
open Game.Entities
open Game.Player
open Game.Transformations
open Procgen

let print_all_entities world =
  List.iter
    (fun ent -> print_endline (GameEntity.string_of_entity ent))
    (GameWorld.all_entities world)

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
            match ent.entity_type with
            | Pigeon _ -> print_string "p "
            | Player -> print_string "@ "
            | Wall -> print_string "# "));
        print_row world (xr + 1, yr) end_x)
    in
    print_row world (x1, y2) x2;
    print_newline ();
    print_world_region world (x1, y1) (x2, y2 - 1)

let print_events (state : GameState.t) =
  ignore (Sys.command "clear");
  print_endline (Printf.sprintf "Event List (%d): " (List.length state.events));
  List.iter (fun event -> print_endline (string_of_event event)) state.events;
  print_string "press any key to continue: ";
  ignore (read_line ())

let rec loop (state : GameState.t) =
  ignore (Sys.command "clear");
  print_world_region state.world (0, 0) (19, 19);
  print_endline ("Turn number " ^ string_of_int state.turn);
  print_string "w/a/s/d/e/q: ";
  let input = String.trim (read_line ()) in
  try
    match input with
    | "w" -> loop (GameState.step state (MovePlayer (0, 1)))
    | "a" -> loop (GameState.step state (MovePlayer (-1, 0)))
    | "s" -> loop (GameState.step state (MovePlayer (0, -1)))
    | "d" -> loop (GameState.step state (MovePlayer (1, 0)))
    | "q" -> ()
    | "e" ->
        print_events state;
        loop state
    | _ -> loop state
  with GameState.Invalid_input _ -> loop state

let () =
  let player = create_default_at Player (0, 0) in
  let pigeon = create_default_at (Pigeon 1) (3, 3) in
  let world =
    GameWorld.put_entity
      (GameWorld.put_entity
         (Procgen.Rwalk.world_from_genworld
            (Rwalk.generate ~printing:false 20 20))
         player)
      pigeon
  in
  let state = GameState.create world [ entity_action_runner ] in
  loop state
