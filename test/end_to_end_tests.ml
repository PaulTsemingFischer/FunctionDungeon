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
 fun (start_state : GameState.t) (entity : GameEntity.t)
     (_ : GameState.input) ->
  let updated =
    GameEntity.update_stats entity
      {
        health = entity.stats.health -. 1.0;
        base_actions = entity.stats.base_actions;
        base_moves = entity.stats.base_moves;
      }
  in
  GameState.set_room start_state
    (GameWorld.put_entity (GameState.room start_state) updated)

let useless_transition (start_state : GameState.t) _ _ : GameState.t =
  start_state

let print_all_entities (w : GameWorld.t) =
  List.iter
    (fun e -> print_endline (GameEntity.string_of_entity e))
    (GameWorld.all_entities w)

(**[create_wall ()] is utility method that creates a wall*)

let create_wall () = create_default_at Wall (0, 0)

(**[generate_starting_state ()] creates a randomly generated, procedural world
   with the player inside the map*)
let generate_starting_state () =
  let player = create_default_at Player (0, 0) in
  let generated_state =
    Transformations.generate_floor player Pgworld.default_room_gen_settings
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
           let player = GameState.get_player state_start in
           print_all_entities (GameState.room state_start);
           assert_equal (Some player)
             (GameWorld.query_id (GameState.room state_start) player.id);
           assert_equal 0 (GameState.get_turn state_start);
           let state_next = GameState.step state_start Wait in
           print_all_entities (GameState.room state_next);
           assert_equal (Some player)
             (GameWorld.query_id (GameState.room state_next) player.id)
             ~printer:string_of_entity_option;
           assert_equal 1 (GameState.get_turn state_next) );
       ]

let _ = run_test_tt_main e2d_tests
