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
           assert_equal [] new_entity.statuses;
           assert_equal 0 new_entity.id ~printer:string_of_int );
         ( "Entity.create produces an entity with id 1 after one entity is \
            already created"
         >:: fun _ ->
           let _unused = make_wall () and new_entity = make_wall () in
           assert_equal { health = 0. } new_entity.stats;
           assert_equal Wall new_entity.entity_type;
           assert_equal (Ascii '#') new_entity.rendering;
           assert_equal [] new_entity.statuses;
           assert_equal 1 new_entity.id ~printer:string_of_int );
       ]

let _ = run_test_tt_main entity_tests
