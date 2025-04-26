open GameDefinitions
open Engine.Utils

type input =
  | MovePlayer of vec2
  | Wait

exception Invalid_input of input

type event =
  | Move of GameEntity.t * vec2 * vec2
  | Say of GameEntity.t * string
  | ChangeHealth of GameEntity.t * float
  | ActivateActionModifier of
      GameEntity.t
      * Modifiers.possible_action list
      * Modifiers.possible_action list
  | ActivateMoveModifier of
      GameEntity.t * Modifiers.possible_move list * Modifiers.possible_move list
  | EntityDeath of GameEntity.t

let string_of_event event =
  match event with
  | Move (entity, startpos, endpos) ->
      Printf.sprintf "%s moved from %s to %s"
        (string_of_type entity.entity_type)
        (string_of_vec2 startpos) (string_of_vec2 endpos)
  | Say (entity, message) ->
      Printf.sprintf "%s says: %s" (string_of_type entity.entity_type) message
  | ActivateActionModifier (entity, _, _) ->
      Printf.sprintf "%s activates their action modifiers!"
        (string_of_type entity.entity_type)
  | ActivateMoveModifier (entity, _, _) ->
      Printf.sprintf "%s activates their move modifiers!"
        (string_of_type entity.entity_type)
  | ChangeHealth (e, amt) ->
      Printf.sprintf "%s's health changed by %.2f"
        (string_of_type e.entity_type)
        amt
  | EntityDeath e -> Printf.sprintf "%s died" (string_of_type e.entity_type)

type t = {
  world : GameWorld.t;
  tiles : GameTiles.t;
  transitions : transition list;
  events : (int * event) list;
  turn : int;
  player : GameWorld.e_t;
  modifiers :
    (string
    * (Modifiers.possible_actions_modifier list
      * Modifiers.possible_moves_modifier list))
    list;
}

and transition = t -> GameEntity.t -> input -> t

let create (world : GameWorld.t) ?(tiles : GameTiles.t = GameTiles.empty)
    (transitions : transition list) : t =
  {
    world;
    tiles;
    transitions;
    events = [];
    turn = 0;
    player =
      GameEntity.create
        {
          health = 10.0;
          base_moves = Modifiers.base_cross_moves;
          base_actions = Modifiers.base_cross_actions;
        }
        Player [] (0, 0);
    modifiers = [];
  }

let print_events state =
  List.iter
    (fun ((turn, event) : int * event) ->
      print_endline (string_of_int turn ^ "\t" ^ string_of_event event))
    (List.rev state.events)

let print_latest_event state =
  if List.length state.events > 0 then
    let turn, event = List.hd state.events in
    print_endline (string_of_int turn ^ "\t" ^ string_of_event event)
  else print_endline "No events"

let query_update_player state =
  let updated_player =
    List.find_opt
      (fun (e : GameEntity.t) -> e.entity_type = Player)
      (GameWorld.all_entities state.world)
  in
  {
    world = state.world;
    tiles = state.tiles;
    transitions = state.transitions;
    events = state.events;
    turn = state.turn;
    player =
      (match updated_player with
      | Some p -> p
      | None -> state.player);
    modifiers = state.modifiers;
  }

let step (state : t) (input : input) =
  let new_state =
    List.fold_left
      (fun (state_ext : t) (transition : transition) ->
        List.fold_left
          (fun (acc : t) (entity : GameEntity.t) ->
            if GameWorld.mem_id acc.world entity.id then
              transition acc entity input
            else acc)
          state_ext
          (GameWorld.all_entities state_ext.world))
      state state.transitions
  in
  print_latest_event state;
  let updated_state =
    {
      world = new_state.world;
      tiles = new_state.tiles;
      transitions = new_state.transitions;
      events = new_state.events;
      turn = new_state.turn + 1;
      player = new_state.player;
      modifiers = new_state.modifiers;
    }
  in
  query_update_player updated_state

let get_world state = state.world
let get_tiles state = state.tiles

let update_world { world; tiles; transitions; events; turn; player; modifiers }
    new_world : t =
  { world = new_world; tiles; transitions; events; turn; player; modifiers }

let update_tiles { world; tiles; transitions; events; turn; player; modifiers }
    new_tiles : t =
  { world; tiles = new_tiles; transitions; events; turn; player; modifiers }

let get_events state = state.events

let add_event { world; tiles; transitions; events; turn; player; modifiers }
    event =
  {
    world;
    tiles;
    transitions;
    events = (turn, event) :: events;
    turn;
    player;
    modifiers;
  }

let get_turn state = state.turn
let get_player state = state.player

exception Entity_not_found of GameEntity.t

let activate_action_modifiers state (entity_type : entity_types)
    (possible_actions : Modifiers.possible_action list) =
  match List.assoc_opt (string_of_type entity_type) state.modifiers with
  | Some (possible_actions_modifiers, _) ->
      List.fold_left
        (fun (possible_actions_acc : Modifiers.possible_action list)
             (action_modifier : Modifiers.possible_actions_modifier) ->
          match action_modifier with
          | ScaleAction factor ->
              List.map
                (fun ((pos, action) : Modifiers.possible_action) ->
                  (scale_vec2 pos factor, action))
                possible_actions_acc)
        possible_actions possible_actions_modifiers
  | None -> possible_actions

let apply_action_modifiers state (entity : GameEntity.t)
    (possible_actions : Modifiers.possible_action list) =
  let modified_actions =
    activate_action_modifiers state entity.entity_type possible_actions
  in
  match List.assoc_opt (string_of_type entity.entity_type) state.modifiers with
  | Some _ ->
      ( add_event state
          (ActivateActionModifier (entity, possible_actions, modified_actions)),
        modified_actions )
  | None -> (state, possible_actions)

let activate_move_modifiers state (entity_type : entity_types)
    (possible_moves : Modifiers.possible_move list) =
  match List.assoc_opt (string_of_type entity_type) state.modifiers with
  | Some (_, possible_moves_modifiers) ->
      let modified_actions =
        List.fold_left
          (fun (possible_moves_acc : Modifiers.possible_move list)
               (action_modifier : Modifiers.possible_moves_modifier) ->
            match action_modifier with
            | ScaleMove factor ->
                List.map
                  (fun (pos : Modifiers.possible_move) -> scale_vec2 pos factor)
                  possible_moves_acc)
          possible_moves possible_moves_modifiers
      in
      modified_actions
  | None -> possible_moves

let apply_move_modifiers state (entity : GameEntity.t)
    (possible_moves : Modifiers.possible_move list) =
  let modified_moves =
    activate_move_modifiers state entity.entity_type possible_moves
  in
  match List.assoc_opt (string_of_type entity.entity_type) state.modifiers with
  | Some _ ->
      ( add_event state
          (ActivateMoveModifier (entity, possible_moves, modified_moves)),
        modified_moves )
  | None -> (state, possible_moves)

let get_modifiers state entity_type :
    Modifiers.possible_actions_modifier list
    * Modifiers.possible_moves_modifier list =
  match List.assoc_opt (string_of_type entity_type) state.modifiers with
  | None -> ([], [])
  | Some x -> x

let add_actions_modifier state possible_action entity_type =
  match List.assoc_opt (string_of_type entity_type) state.modifiers with
  | None ->
      {
        world = state.world;
        tiles = state.tiles;
        transitions = state.transitions;
        events = state.events;
        player = state.player;
        turn = state.turn;
        modifiers =
          (string_of_type entity_type, ([ possible_action ], []))
          :: state.modifiers;
      }
  | Some (possible_action_list, movement_modifier_list) ->
      let removed_modifier_assoc =
        List.remove_assoc (string_of_type entity_type) state.modifiers
      in
      {
        world = state.world;
        tiles = state.tiles;
        transitions = state.transitions;
        events = state.events;
        player = state.player;
        turn = state.turn;
        modifiers =
          ( string_of_type entity_type,
            (possible_action :: possible_action_list, movement_modifier_list) )
          :: removed_modifier_assoc;
      }

let add_moves_modifier state movement_modifier entity_type =
  match List.assoc_opt (string_of_type entity_type) state.modifiers with
  | None ->
      {
        world = state.world;
        tiles = state.tiles;
        transitions = state.transitions;
        events = state.events;
        player = state.player;
        turn = state.turn;
        modifiers =
          (string_of_type entity_type, ([], [ movement_modifier ]))
          :: state.modifiers;
      }
  | Some (possible_action_list, movement_modifier_list) ->
      let removed_modifier_assoc =
        List.remove_assoc (string_of_type entity_type) state.modifiers
      in
      {
        world = state.world;
        tiles = state.tiles;
        transitions = state.transitions;
        events = state.events;
        player = state.player;
        turn = state.turn;
        modifiers =
          ( string_of_type entity_type,
            (possible_action_list, movement_modifier :: movement_modifier_list)
          )
          :: removed_modifier_assoc;
      }

let remove_actions_modifier state entity_type =
  match List.assoc_opt (string_of_type entity_type) state.modifiers with
  | None ->
      {
        world = state.world;
        tiles = state.tiles;
        transitions = state.transitions;
        events = state.events;
        player = state.player;
        turn = state.turn;
        modifiers = state.modifiers;
      }
  | Some (possible_action_list, movement_modifier_list) ->
      let removed_modifier_assoc =
        List.remove_assoc (string_of_type entity_type) state.modifiers
      in
      {
        world = state.world;
        tiles = state.tiles;
        transitions = state.transitions;
        events = state.events;
        player = state.player;
        turn = state.turn;
        modifiers =
          ( string_of_type entity_type,
            match possible_action_list with
            | h :: t -> (t, movement_modifier_list)
            | [] -> ([], movement_modifier_list) )
          :: removed_modifier_assoc;
      }

let add_obstacle_to_world state world pos (obstacle_type : Obstacles.obstacle) =
  let new_obstacle = create_default_at (Obstacle obstacle_type) pos in
  update_world state (GameWorld.put_entity world new_obstacle)

let positions_in_radius (center : vec2) (radius : int) : vec2 list =
  let center_x, center_y = center in
  let positions = ref [] in

  for x = center_x - radius to center_x + radius do
    for y = center_y - radius to center_y + radius do
      let pos = (x, y) in

      let dx = x - center_x in
      let dy = y - center_y in
      let distance_squared = (dx * dx) + (dy * dy) in

      if distance_squared <= radius * radius then positions := pos :: !positions
    done
  done;

  !positions

let build_barrier (state : t) (world : GameWorld.t) (center : vec2)
    (radius : int) (objects : Obstacles.obstacle) =
  let positions = positions_in_radius center radius in
  List.fold_left
    (fun current_state p -> add_obstacle_to_world current_state world p objects)
    state positions
