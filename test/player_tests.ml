open OUnit2
open Engine
open Game
open Player
open GameDefinitions
open GameState

let blank_world = GameWorld.empty
let origin_player = create_default_at Player (0, 0)

let origin_player_world =
  GameWorld.put_entity blank_world (to_gameworld_type origin_player)

let run_player_action_state pigeon_pos input =
  let test_pigeon = create_default_at Pigeon pigeon_pos in
  let test_world =
    GameWorld.put_entity origin_player_world (to_gameworld_type test_pigeon)
  in
  let test_state =
    GameState.create [ test_world ] [ GameTiles.empty ] []
      (to_gameworld_type origin_player)
      0
  in
  player_action test_state origin_player input

let rec find_pigeon_health entity_lst =
  match entity_lst with
  | [] -> failwith "no pigeon found"
  | e :: tl ->
      if e.GameEntity.entity_type = to_entity_type Pigeon then
        (to_entity_stats e.GameEntity.stats).health
      else find_pigeon_health tl

let base_list_size =
  List.length (GameWorld.all_entities origin_player_world) + 1

let base_pigeon_health =
  find_pigeon_health
    (List.map
       (fun x -> to_gameentity_type x)
       (GameWorld.all_entities
          (GameWorld.put_entity origin_player_world
             (to_gameworld_type (create_default_at Pigeon (0, 1))))))

let player_attack_pigeon_pos_health =
  find_pigeon_health
    (List.map
       (fun x -> to_gameentity_type x)
       (GameWorld.all_entities
          (GameState.room (run_player_action_state (0, 1) Attack))))

let player_attack_pigeon_pos_test =
  "A pigeon will have less health after a player attacks it" >:: fun _ ->
  assert_equal true
    (base_pigeon_health > player_attack_pigeon_pos_health)
    ~printer:string_of_bool

let player_attack_pigeon_neg_health =
  find_pigeon_health
    (List.map
       (fun x -> to_gameentity_type x)
       (GameWorld.all_entities
          (GameState.room (run_player_action_state (0, 2) Attack))))

let player_attack_pigeon_neg_test =
  "A pigeon will have the same health if a player tries to attack it but is \
   out of range"
  >:: fun _ ->
  assert_equal base_pigeon_health player_attack_pigeon_neg_health
    ~printer:string_of_float

let tests =
  "test suite"
  >::: [ player_attack_pigeon_pos_test; player_attack_pigeon_neg_test ]

let _ = run_test_tt_main tests
