open OUnit2
open Engine
open Game
open Enemytype
open Enemyaction

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

let string_of_variable_range_test =
  "String of variable range enemy test" >:: fun _ ->
  assert_equal "variable range enemy" (string_of_enemy (Variable_Range 4))
    ~printer:(fun x -> x)

let string_of_variable_damage_test =
  "String of variable damage enemy test" >:: fun _ ->
  assert_equal "variable damage enemy" (string_of_enemy (Variable_Damage 4.))
    ~printer:(fun x -> x)

let string_of_variable_damage_range_test =
  "String of variable range and damage enemy test" >:: fun _ ->
  assert_equal "variable range and damage enemy"
    (string_of_enemy (Variable_Range_and_Damage (2, 4.)))
    ~printer:(fun x -> x)

let tests =
  "test suite"
  >::: [
         string_of_jailer_test;
         string_of_thief_test;
         string_of_fog_cloud_test;
         string_of_variable_range_test;
         string_of_variable_damage_test;
         string_of_variable_damage_range_test;
       ]

let _ = run_test_tt_main tests
