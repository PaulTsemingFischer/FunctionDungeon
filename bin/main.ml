open Engine

type State.input += Move of Entity.vec2

let create_wall_at = Entity.create { health = 0. } Wall Entity.Id_debug []

let print_all_entities world =
  List.iter
    (fun ent -> print_endline (Entity.string_of_entity ent))
    (World.all_entities world)

let is_pos_empty world pos =
  match World.query_pos world pos with
  | Some _ -> false
  | None -> true

let player_action_generator (state : State.t) (entity : Entity.t) input =
  match input with
  | Move dir ->
      if not (is_pos_empty state.world (Entity.add_vec2 entity.pos dir)) then
        raise (State.Invalid_input input)
      else
        Some
          ( 0,
            fun state : State.t ->
              State.update_world state
                (World.put_entity state.world
                   (Entity.set_pos entity (Entity.add_vec2 dir entity.pos))) )
  | _ -> None

let entity_action_generator =
  State.Generator
    (fun (state : State.t) (entity : Entity.t) (input : State.input) ->
      match entity.entity_type with
      | Entity.Player -> player_action_generator state entity input
      | Entity.Wall -> None)

let rec print_world_region (world : World.t) ((x1, y1) : int * int)
    ((x2, y2) : int * int) =
  if y1 > y2 then ()
  else
    let rec print_row (world : World.t) ((xr, yr) : int * int) (end_x : int) =
      if xr > end_x then ()
      else (
        (match World.query_pos world (xr, yr) with
        | None -> print_string (String.make 1 '.' ^ " ")
        | Some ent -> (
            match ent.rendering with
            | Ascii x -> print_string (String.make 1 x ^ " ")
            | Id_debug -> Printf.printf "%-2s" (Entity.string_of_id ent.id)));
        print_row world (xr + 1, yr) end_x)
    in
    print_row world (x1, y2) x2;
    print_newline ();
    print_world_region world (x1, y1) (x2, y2 - 1)

let rec loop (state : State.t) =
  ignore (Sys.command "clear");
  print_world_region state.world (-5, -5) (10, 10);
  let input = String.trim (read_line ()) in
  try
    match input with
    | "w" -> loop (State.step state (Move (0, 1)))
    | "a" -> loop (State.step state (Move (-1, 0)))
    | "s" -> loop (State.step state (Move (0, -1)))
    | "d" -> loop (State.step state (Move (1, 0)))
    | "q" -> ()
    | _ -> loop state
  with State.Invalid_input input_val -> (
    match input_val with
    | Move vec -> loop state
    | _ -> print_endline "unknown invalid action")

let () =
  let player =
    Entity.create { health = 1. } Entity.Player (Entity.Ascii '@') [] (0, 0)
  in
  let world = World.put_entity World.empty player in
  let world_with_walls =
    List.fold_left
      (fun (current_world : World.t) wall_pos ->
        World.put_entity current_world (create_wall_at wall_pos))
      world
      (List.init 10 (fun x -> (-5 + x, 5)))
  in
  let state = State.create world_with_walls [ entity_action_generator ] in
  loop state
