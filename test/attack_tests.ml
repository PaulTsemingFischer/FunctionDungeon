open OUnit2
open Engine
open Game

let make_modify_test name expected_output func input =
  name >:: fun _ ->
  let output = Item.modify_attack func input in
  assert_equal expected_output output

let tests =
  "test suite"
  >::: [
         make_modify_test "Add 1 damage to empty"
           Item.AttackMap.(
             empty |> add (Item.to_key (0, 0)) [ Modifiers.DealDamage 1.0 ])
           (fun tile -> [ (fst tile, [ Modifiers.DealDamage 1.0 ]) ])
           Item.AttackMap.(empty |> add (Item.to_key (0, 0)) []);
         make_modify_test "Add 1 damage on top of existing effects"
           Item.AttackMap.(
             empty
             |> add
                  (Item.to_key (0, 0))
                  [ Modifiers.ApplyFire (1., 3); Modifiers.DealDamage 1.0 ])
           (fun tile -> [ (fst tile, [ Modifiers.DealDamage 1.0 ]) ])
           Item.AttackMap.(
             empty |> add (Item.to_key (0, 0)) [ Modifiers.ApplyFire (1., 3) ]);
         make_modify_test "Augment to above tile"
           Item.AttackMap.(
             empty |> add (Item.to_key (0, 0)) [] |> add (Item.to_key (0, 1)) [])
           (fun tile -> [ tile; (Utils.add_vec2 (fst tile) (0, 1), snd tile) ])
           Item.AttackMap.(empty |> add (Item.to_key (0, 0)) []);
         make_modify_test "Augment to above tile with duplicates"
           Item.AttackMap.(
             empty
             |> add (Item.to_key (0, 0)) []
             |> add (Item.to_key (0, 1)) []
             |> add (Item.to_key (0, 2)) [])
           (fun tile -> [ tile; (Utils.add_vec2 (fst tile) (0, 1), snd tile) ])
           Item.AttackMap.(
             empty |> add (Item.to_key (0, 0)) [] |> add (Item.to_key (0, 1)) []);
       ]

let _ = run_test_tt_main tests
