open OUnit2
open Engine
open Game
open Enemytype
open Enemyaction
open GameDefinitions
open GameState

let string_of_jailer_test =
  "String of jailer test" >:: fun _ ->
  assert_equal "jailer" (string_of_enemy (Jailer (4, 2))) ~printer:(fun x -> x)

let string_of_thief_test =
  "String of thief test" >:: fun _ ->
  assert_equal "thief" (string_of_enemy Thief) ~printer:(fun x -> x)

let string_of_fog_cloud_test =
  "String of fog cloud test" >:: fun _ ->
  assert_equal "fog cloud"
    (string_of_enemy (Fog_Cloud (4, 2)))
    ~printer:(fun x -> x)

let string_of_variable_damage_range_test =
  "String of variable range and damage enemy test" >:: fun _ ->
  assert_equal "enemy: range=2 damage=4.00"
    (string_of_enemy (Variable_Range_and_Damage (2, 4.)))
    ~printer:(fun x -> x)

let blank_world = GameWorld.empty
let origin_player = create_default_at Player (0, 0)

let origin_player_world =
  GameWorld.put_entity blank_world (to_gameworld_type origin_player)

let run_enemy_action_state enemy pos =
  let test_enemy = create_default_at (Enemy enemy) pos in
  let test_world =
    GameWorld.put_entity origin_player_world (to_gameworld_type test_enemy)
  in
  let test_state =
    GameState.create [ test_world ] [ GameTiles.empty ] []
      (to_gameworld_type origin_player)
      0
  in
  enemy_action test_state test_enemy enemy ()

let base_list_size =
  List.length (GameWorld.all_entities origin_player_world) + 1

let post_jailer_attack_list_pos =
  GameWorld.all_entities
    (GameState.room (run_enemy_action_state jailer_small (0, 1)))

let post_jailer_attack_list_neg =
  GameWorld.all_entities
    (GameState.room (run_enemy_action_state jailer_small (0, 2)))

let jailer_action_entity_list_length_test_pos =
  "There are more entities present after jailer attacks than before"
  >:: fun _ ->
  assert_equal true
    (List.length post_jailer_attack_list_pos > base_list_size)
    ~printer:string_of_bool

let jailer_action_entity_list_length_test_neg =
  "The number of entities stays the same if the jailer is not close enough in \
   range to attack"
  >:: fun _ ->
  assert_equal base_list_size
    (List.length post_jailer_attack_list_neg)
    ~printer:string_of_int

let thief_action_state pos : GameState.t =
  let test_enemy = create_default_at (Enemy Thief) pos in
  let test_world =
    GameWorld.put_entity origin_player_world (to_gameworld_type test_enemy)
  in
  let test_state =
    GameState.create [ test_world ] [ GameTiles.empty ] []
      (to_gameworld_type origin_player)
      0
  in
  let test_state_modifier =
    add_actions_modifier test_state (AddDamage 2.) Player
  in
  enemy_action test_state_modifier test_enemy Thief ()

let thief_action_state_attack_modifiers =
  let state = thief_action_state (0, 1) in
  GameState.get_modifiers state (to_entity_type Player)

let thief_action_state_no_attack_modifiers =
  let state = thief_action_state (0, 2) in
  GameState.get_modifiers state (to_entity_type Player)

let thief_attack_modifiers_pos =
  "There are no modifiers left after a thief steals from a player with one \
   modifer"
  >:: fun _ ->
  assert_equal true
    (List.length (fst thief_action_state_attack_modifiers) < 1)
    ~printer:string_of_bool

let thief_attack_modifiers_neg =
  "A player will not lose any modifiers if the thief is too far away to attack"
  >:: fun _ ->
  assert_equal true
    (List.length (fst thief_action_state_no_attack_modifiers) = 1)
    ~printer:string_of_bool

let tests =
  "test suite"
  >::: [
         string_of_jailer_test;
         string_of_thief_test;
         string_of_fog_cloud_test;
         string_of_variable_damage_range_test;
         jailer_action_entity_list_length_test_pos;
         jailer_action_entity_list_length_test_neg;
         thief_attack_modifiers_pos;
         thief_attack_modifiers_neg;
       ]

let _ = run_test_tt_main tests
