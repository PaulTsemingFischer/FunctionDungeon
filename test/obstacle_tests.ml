open OUnit2
open Engine
open Game
open Obstacles

let fence_age_test =
  "Fence age test" >:: fun _ ->
  assert_equal (Fence 2) (update_obstacle_age (Fence 1))

let spreading_growth_test_rate_1 =
  "Spreading fire growth test rate 1" >:: fun _ ->
  assert_equal
    (Spreading_Fire ((0, 0), 5, 1))
    (grow_fire (Spreading_Fire ((0, 0), 4, 1)))

let spreading_growth_test_rate_2 =
  "Spreading fire growth test rate 1" >:: fun _ ->
  assert_equal
    (Spreading_Fire ((0, 0), 8, 2))
    (grow_fire (Spreading_Fire ((0, 0), 4, 2)))

let tests = "test suite" >::: [ fence_age_test ]
let _ = run_test_tt_main tests
