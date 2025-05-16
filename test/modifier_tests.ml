open OUnit2
open Engine
open Game

(** [attacks_eq lst1 lst2] compares [lst1] and [lst2] for equality. *)
let rec attacks_eq lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> true
  | h :: t, h2 :: t2 ->
      if not (Item.compare_effects h h2) then false else attacks_eq t t2
  | _, _ -> false

(** [lists_eq lst1 lst2] compares [lst1] and [lst2] for equality. *)
let rec lists_eq (lst1 : Modifiers.possible_action list)
    (lst2 : Modifiers.possible_action list) =
  match (lst1, lst2) with
  | [], [] -> true
  | h :: t, h2 :: t2 ->
      if
        fst (fst h) <> fst (fst h2)
        || snd (fst h) <> snd (fst h2)
        || not (attacks_eq (snd h) (snd h2))
      then false
      else lists_eq t t2
  | _, _ -> false

(** [string_of_action_list lst] converts contents of [lst] to string format. *)
let rec string_of_action_list (lst : Modifiers.possible_action list) =
  match lst with
  | [] -> ""
  | h :: t ->
      let v = fst h in
      "("
      ^ string_of_int (fst v)
      ^ ", "
      ^ string_of_int (snd v)
      ^ ") ["
      ^ Item.effects_to_string (snd h)
      ^ "]\n" ^ string_of_action_list t

(** [make_equals_test name expected_output actual_output] is a unit test with
    name [name] that asserts whether [expected_output] and [actual_output] are
    equal. *)
let make_equals_test name expected_output actual_output =
  name >:: fun _ ->
  assert_equal expected_output actual_output ~cmp:lists_eq
    ~printer:string_of_action_list

let tests =
  "test suite"
  >::: [
         make_equals_test "Variable range 1 & damage 2"
           (Modifiers.enemy_circle_actions (Variable_Range_and_Damage (1, 2.)))
           [
             ((1, 0), [ Modifiers.DealDamage 2. ]);
             ((-1, 0), [ Modifiers.DealDamage 2. ]);
             ((1, 0), [ Modifiers.DealDamage 2. ]);
             ((-1, 0), [ Modifiers.DealDamage 2. ]);
             ((1, 1), [ Modifiers.DealDamage 2. ]);
             ((-1, 1), [ Modifiers.DealDamage 2. ]);
             ((1, -1), [ Modifiers.DealDamage 2. ]);
             ((-1, -1), [ Modifiers.DealDamage 2. ]);
           ];
         make_equals_test "Variable range 0 & damage 2"
           (Modifiers.enemy_circle_actions (Variable_Range_and_Damage (0, 2.)))
           [];
         make_equals_test "Thief attack"
           (Modifiers.enemy_circle_actions Thief)
           [
             ((1, 0), [ Modifiers.StealAttack ]);
             ((-1, 0), [ Modifiers.StealAttack ]);
             ((0, 1), [ Modifiers.StealAttack ]);
             ((0, -1), [ Modifiers.StealAttack ]);
           ];
         make_equals_test "Jailer attack"
           (Modifiers.enemy_circle_actions (Jailer (5, 2)))
           [
             ((1, 0), [ Modifiers.BarrierAttack (5, Obstacles.Fence 2) ]);
             ((-1, 0), [ Modifiers.BarrierAttack (5, Obstacles.Fence 2) ]);
             ((0, 1), [ Modifiers.BarrierAttack (5, Obstacles.Fence 2) ]);
             ((0, -1), [ Modifiers.BarrierAttack (5, Obstacles.Fence 2) ]);
           ];
         make_equals_test "Thief attack"
           (Modifiers.enemy_circle_actions (Fog_Cloud (1, 2)))
           [
             ((1, 0), [ Modifiers.FogAttack (1, 2) ]);
             ((-1, 0), [ Modifiers.FogAttack (1, 2) ]);
             ((0, 1), [ Modifiers.FogAttack (1, 2) ]);
             ((0, -1), [ Modifiers.FogAttack (1, 2) ]);
           ];
       ]

let _ = run_test_tt_main tests
