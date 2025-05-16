open OUnit2
open Game
open Game.GameDefinitions
open Game.GameState

let misc_tests =
  "test suite"
  >::: [
         ("Mud tile" >:: fun _ -> assert_equal "mud" (string_of_tile_type Mud));
         ( "ground tile" >:: fun _ ->
           assert_equal "ground" (string_of_tile_type Ground) );
         ("Fire" >:: fun _ -> assert_equal "fire" (string_of_type Fire));
         ( "HorizontalBouncer" >:: fun _ ->
           assert_equal "h-bouncer" (string_of_type (HorizontalBouncer true)) );
         ( "fence" >:: fun _ ->
           assert_equal "fence" (string_of_type (Obstacle (Obstacles.Fence 3)))
         );
         ( "SpecialItem" >:: fun _ ->
           assert_equal "special-item" (string_of_type SpecialItem) );
         ( "ApplyFire" >:: fun _ ->
           assert_equal
             "player received fire status effect that does 2.00 damage for 3 \
              turns"
             (string_of_event
                (ApplyFire
                   ( GameEntity.create
                       (to_gameentity_stats
                          { health = 10.0; base_moves = []; base_actions = [] })
                       (to_entity_type Player) [] (0, 0),
                     (2., 3) ))) );
         ( "activate action modifiers" >:: fun _ ->
           assert_equal "player activates their action modifiers!"
             (string_of_event
                (ActivateActionModifier
                   ( GameEntity.create
                       (to_gameentity_stats
                          { health = 10.0; base_moves = []; base_actions = [] })
                       (to_entity_type Player) [] (0, 0),
                     [],
                     [] ))) );
         ( "take fire dmg" >:: fun _ ->
           assert_equal "player took damage from fire status effect"
             (string_of_event
                (TakeFireDamage
                   (GameEntity.create
                      (to_gameentity_stats
                         { health = 10.0; base_moves = []; base_actions = [] })
                      (to_entity_type Player) [] (0, 0)))) );
         ( "pick up special item" >:: fun _ ->
           assert_equal "player picked up a special item! Current count: 3"
             (string_of_event
                (PickUpSpecial
                   ( GameEntity.create
                       (to_gameentity_stats
                          { health = 10.0; base_moves = []; base_actions = [] })
                       (to_entity_type Player) [] (0, 0),
                     3 ))) );
         ( "entity death" >:: fun _ ->
           assert_equal "player died"
             (string_of_event
                (EntityDeath
                   (GameEntity.create
                      (to_gameentity_stats
                         { health = 10.0; base_moves = []; base_actions = [] })
                      (to_entity_type Player) [] (0, 0)))) );
         ( "fog cloud" >:: fun _ ->
           assert_equal "fog cloud of 6 radius for 5 frames"
             (string_of_event
                (FogCloud
                   ( GameEntity.create
                       (to_gameentity_stats
                          { health = 10.0; base_moves = []; base_actions = [] })
                       (to_entity_type Player) [] (0, 0),
                     6,
                     5 ))) );
       ]

let _ = run_test_tt_main misc_tests
