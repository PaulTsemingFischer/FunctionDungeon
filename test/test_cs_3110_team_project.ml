open OUnit2
open Engine.Entity

let make_wall () = create { health = 0. } Wall (Ascii '#') []

(**[entity_tests] tests functionality related to creating entities*)
let entity_tests =
  "test suite that tests basic operations of the Entity module"
  >::: [
         ( "Entity.create produces an entity with id 0 and the provided entity \
            information when no other entities have been created"
         >:: fun _ ->
           let new_entity = make_wall () in
           assert_equal { health = 0. } new_entity.stats;
           assert_equal Wall new_entity.entity_type;
           assert_equal (Ascii '#') new_entity.rendering;
           assert_equal [] new_entity.statuses );
         ( "Entity.create produces an entity with a different id after one \
            entity is already created"
         >:: fun _ ->
           let e_1 = make_wall () and e_2 = make_wall () in
           assert_equal { health = 0. } e_2.stats;
           assert_equal Wall e_2.entity_type;
           assert_equal (Ascii '#') e_2.rendering;
           assert_equal [] e_2.statuses;
           assert_bool "entity id should be different, but is the same"
             (not (e_1.id = e_2.id)) );
       ]

let _ = run_test_tt_main entity_tests
