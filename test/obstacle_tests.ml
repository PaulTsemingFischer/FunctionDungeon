open OUnit2
open Engine
open Game
open Obstacles
open GameDefinitions
open GameState
open Obstacleaction

let string_of_vec2 v =
  match v with
  | x, y -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let string_of_fence_test =
  "String of fence test" >:: fun _ ->
  assert_equal "fence" (string_of_obstacle (Fence 1)) ~printer:(fun x -> x)

let string_of_spreading_fire_test =
  "String of spreading fire test" >:: fun _ ->
  assert_equal "spreading fire"
    (string_of_obstacle (Spreading_Fire ((0, 0), 4, 1)))
    ~printer:(fun x -> x)

let get_center_test =
  "Get center test" >:: fun _ ->
  assert_equal (0, 0)
    (get_c (Spreading_Fire ((0, 0), 4, 1)))
    ~printer:string_of_vec2

let get_center_test_fence =
  "Get center test fence" >:: fun _ ->
  assert_raises WrongObsType (fun _ -> get_c (Fence 1))

let get_radius_test =
  "Get radius test" >:: fun _ ->
  assert_equal 4 (get_r (Spreading_Fire ((0, 0), 4, 1))) ~printer:string_of_int

let get_radius_test_fence =
  "Get radius test fence" >:: fun _ ->
  assert_raises WrongObsType (fun _ -> get_r (Fence 1))

let get_growth_rate_test =
  "Get growth rate test" >:: fun _ ->
  assert_equal 1 (get_g (Spreading_Fire ((0, 0), 4, 1)))

let get_growth_rate_test_fence =
  "Get growth rate test fence" >:: fun _ ->
  assert_raises WrongObsType (fun _ -> get_g (Fence 1))

let fence_age_test =
  "Fence age test" >:: fun _ ->
  assert_equal (Fence 1) (update_obstacle_age (Fence 2))

let spreading_fire_age_test =
  "Spreading fire age test" >:: fun _ ->
  assert_raises WrongObsType (fun _ ->
      update_obstacle_age (Spreading_Fire ((0, 0), 4, 1)))

let spreading_growth_test_rate_1 =
  "Spreading fire growth test rate 1" >:: fun _ ->
  assert_equal
    (Spreading_Fire ((0, 0), 5, 1))
    (grow_fire (Spreading_Fire ((0, 0), 4, 1)))

let spreading_growth_test_rate_2 =
  "Spreading fire growth test rate 2" >:: fun _ ->
  assert_equal
    (Spreading_Fire ((0, 0), 6, 2))
    (grow_fire (Spreading_Fire ((0, 0), 4, 2)))

let fence_growth_test =
  "Fence growth test assert raises" >:: fun _ ->
  assert_raises WrongObsType (fun _ -> grow_fire (Fence 1))

let blank_world = GameWorld.empty
let origin_player = create_default_at Player (0, 0)

let origin_player_world =
  GameWorld.put_entity blank_world (to_gameworld_type origin_player)

let run_obstacle_action_state obstacle =
  let test_obstacle = create_default_at (Obstacle obstacle) (2, 2) in
  let test_world =
    GameWorld.put_entity origin_player_world (to_gameworld_type test_obstacle)
  in
  let test_state =
    GameState.create [ test_world ] [ GameTiles.empty ] []
      (to_gameworld_type origin_player)
      0
  in
  obstacle_action test_state test_obstacle obstacle ()

let base_list_size =
  List.length (GameWorld.all_entities origin_player_world) + 1

let fence_dying_entities =
  GameWorld.all_entities (GameState.room (run_obstacle_action_state (Fence 0)))

let fence_aging_entities =
  GameWorld.all_entities (GameState.room (run_obstacle_action_state (Fence 1)))

let fence_dying_test =
  "A fence with 0 turns left will die and therefore there will be less \
   entities present"
  >:: fun _ ->
  assert_equal true
    (List.length fence_dying_entities < base_list_size)
    ~printer:string_of_bool

let fence_aging_test =
  "A fence with 1 turn left will remain and therefore there will be the same \
   number of\n\
  \   entities present"
  >:: fun _ -> assert_equal (List.length fence_aging_entities) base_list_size

let spreading_fire_entities =
  GameWorld.all_entities
    (GameState.room (run_obstacle_action_state (Spreading_Fire ((2, 2), 1, 1))))

let spreading_fire_test =
  "A world with a spreading fire will have more entities after that fire takes \
   an action and expands"
  >:: fun _ ->
  assert_equal true
    (List.length spreading_fire_entities > base_list_size)
    ~printer:string_of_bool

let tests =
  "test suite"
  >::: [
         string_of_fence_test;
         string_of_spreading_fire_test;
         get_center_test;
         get_center_test_fence;
         get_radius_test;
         get_radius_test_fence;
         get_growth_rate_test;
         get_growth_rate_test_fence;
         fence_age_test;
         spreading_fire_age_test;
         spreading_growth_test_rate_1;
         spreading_growth_test_rate_2;
         fence_growth_test;
         fence_dying_test;
         fence_aging_test;
         spreading_fire_test;
       ]

let _ =
  print_endline "obstacle tests:";
  run_test_tt_main tests
