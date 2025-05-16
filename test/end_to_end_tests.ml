open OUnit2
open Game
open Procgen
open Game.GameDefinitions

(**[string_of_entity_option op] converts [op] into a string*)
let string_of_entity_option (op : GameEntity.t option) =
  match op with
  | Some x -> GameEntity.string_of_entity x
  | None -> "none"

let simple_transition : GameState.transition =
 fun (start_state : GameState.t) (entity : GameWorld.e_t)
     (_ : GameState.input) ->
  let entity = to_gameentity_type entity in
  let stats = to_entity_stats entity.stats in
  let updated =
    GameEntity.update_stats entity
      (to_gameentity_stats
         {
           health = stats.health -. 1.0;
           base_actions = stats.base_actions;
           base_moves = stats.base_moves;
         })
  in
  GameState.set_room start_state
    (GameWorld.put_entity
       (GameState.room start_state)
       (to_gameworld_type updated))

let useless_transition (start_state : GameState.t) _ _ : GameState.t =
  start_state

let print_all_entities (w : GameWorld.t) =
  List.iter
    (fun e -> print_endline (GameEntity.string_of_entity e))
    (List.map (fun x -> to_gameentity_type x) (GameWorld.all_entities w))

(**[create_wall ()] is utility method that creates a wall*)

let create_wall () = create_default_at Wall (0, 0)

(**[generate_starting_state ()] creates a randomly generated, procedural world
   with the player inside the map*)
let generate_starting_state () =
  let player = create_default_at Player (0, 0) in
  let generated_state =
    Transformations.generate_floor (to_gameworld_type player)
      Pgworld.default_room_gen_settings
      [ Transitions.entity_status_runner; Transitions.entity_action_runner ]
  in
  GameState.add_moves_modifier generated_state (ScaleMove 1) Pigeon
  |> GameState.query_update_player

(**[e2d_tests] tests stepping through state with multiple state generator
   functions and ensuring that the state at each step is the expected state*)
let e2d_tests =
  "test suite that tests end-to-end functionality of state progression, with \
   procedural generation and full entity set"
  >::: [
         ( "stepping through the state of a game with one generator that does \
            nothing\n\
           \  and a single entity advances the turn but leaves the entity \
            unchanged"
         >:: fun _ ->
           let state_start = generate_starting_state () in
           let player = to_gameentity_type (GameState.get_player state_start) in
           print_all_entities (GameState.room state_start);
           assert_equal (Some player)
             (match
                GameWorld.query_id
                  (GameState.room state_start)
                  (to_entity_id player.id)
              with
             | None -> None
             | Some player -> Some (to_gameentity_type player));
           assert_equal 0 (GameState.get_turn state_start);
           let state_next = GameState.step state_start Wait in
           print_all_entities (GameState.room state_next);
           assert_equal (Some player)
             (match
                GameWorld.query_id
                  (GameState.room state_next)
                  (to_entity_id player.id)
              with
             | None -> None
             | Some player -> Some (to_gameentity_type player))
             ~printer:string_of_entity_option;
           assert_equal 1 (GameState.get_turn state_next) );
         ( "Moving the player to pick up modifiers adds them to the modifier \
            stack for player"
         >:: fun _ ->
           let player = create_default_at Player (0, 0) in
           let pre_1_world =
             GameWorld.put_entity GameWorld.empty (to_gameworld_type player)
           in
           let pre_2_world =
             GameWorld.put_entity pre_1_world
               (to_gameworld_type (create_default_at (HealthItem 10.0) (1, 0)))
           in
           let pre_3_world =
             GameWorld.put_entity pre_2_world
               (to_gameworld_type
                  (create_default_at (ModifierItem (AddDamage 1.0)) (2, 0)))
           in
           let state_start =
             GameState.create [ pre_3_world ] []
               [ Transitions.entity_action_runner ]
               (to_gameworld_type player) 0
           in
           let state_1 =
             GameState.step state_start (GameState.MovePlayer (1, 0))
           in
           let inv_input = GameState.Act (2, 0) in
           assert_raises (GameState.Invalid_input inv_input) (fun _ ->
               GameState.step state_1 inv_input);
           let state_2 = GameState.step state_1 (GameState.MovePlayer (1, 0)) in

           assert_equal 1
             (List.length
                (fst (GameState.get_modifiers state_2 (to_entity_type Player))));
           assert_equal 2 (GameState.get_turn state_2) );
         ( "Moving the player into random, non-movable objects like walls, \
            rocks, water, etc. prevents them from moving"
         >:: fun _ ->
           let player = create_default_at Player (0, 0) in
           let pre_1_world =
             GameWorld.put_entity GameWorld.empty (to_gameworld_type player)
           in
           let pre_2_world =
             GameWorld.put_entity pre_1_world
               (to_gameworld_type (create_default_at Rock (1, 0)))
           in
           let pre_3_world =
             GameWorld.put_entity pre_2_world
               (to_gameworld_type (create_default_at Wall (-1, 0)))
           in
           let pre_4_world =
             GameWorld.put_entity pre_3_world
               (to_gameworld_type
                  (create_default_at (Obstacle (Fence 100)) (0, 1)))
           in
           let pre_5_world =
             GameWorld.put_entity pre_4_world
               (to_gameworld_type (create_default_at Water (0, -1)))
           in
           let state_start =
             GameState.create [ pre_5_world ] []
               [ Transitions.entity_action_runner ]
               (to_gameworld_type player) 0
           in
           let inv_input = GameState.MovePlayer (1, 0) in
           assert_raises (GameState.Invalid_input inv_input) (fun _ ->
               GameState.step state_start inv_input);
           let inv_input = GameState.MovePlayer (-1, 0) in
           assert_raises (GameState.Invalid_input inv_input) (fun _ ->
               GameState.step state_start inv_input);
           let inv_input = GameState.MovePlayer (0, 1) in
           assert_raises (GameState.Invalid_input inv_input) (fun _ ->
               GameState.step state_start inv_input);
           let inv_input = GameState.MovePlayer (0, -1) in
           assert_raises (GameState.Invalid_input inv_input) (fun _ ->
               GameState.step state_start inv_input) );
       ]

let _ = run_test_tt_main e2d_tests
