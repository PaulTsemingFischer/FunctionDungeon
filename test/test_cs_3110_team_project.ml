open OUnit2
open Engine.Entity
open Engine.World

(**[create_wall ()] is utility method that creates a wall entity*)
let create_wall () = create { health = 0. } Wall (Ascii '#') [] (0, 0)

(**[string_of_entity_option op] converts [op] into a string*)
let string_of_entity_option (op : Engine.Entity.t option) =
  match op with
  | Some x -> string_of_entity x
  | None -> "none"

(**[entity_tests] tests functionality related to creating entities*)
let entity_tests =
  "test suite that tests basic operations of the Entity module"
  >::: [
         ( "Entity.create produces an entity with id 0 and the provided entity \
            information when no other entities have been created"
         >:: fun _ ->
           let new_entity = create_wall () in
           assert_equal { health = 0. } new_entity.stats;
           assert_equal Wall new_entity.entity_type;
           assert_equal (Ascii '#') new_entity.rendering;
           assert_equal [] new_entity.statuses );
         ( "Entity.create produces an entity with a different id after one \
            entity is already created"
         >:: fun _ ->
           let e_1 = create_wall () and e_2 = create_wall () in
           assert_equal { health = 0. } e_2.stats;
           assert_equal Wall e_2.entity_type;
           assert_equal (Ascii '#') e_2.rendering;
           assert_equal [] e_2.statuses;
           assert_bool "entity id should be different, but is the same"
             (not (e_1.id = e_2.id)) );
       ]

(**[world_tests] tests functionality related to creating worlds, adding entities
   into worlds, updating worlds, and removing entities*)
let world_tests =
  "test suite that tests creating worlds, adding entities into worlds, \
   updating entities, and removing entities"
  >::: [
         ( "creating an empty world and retrieving all entities in the world \
            returns an empty list of entities"
         >:: fun _ -> assert_equal [] (all_entities empty) );
         ( "adding a single entity to a world and retrieving all entities \
            should return a list containing only that entity"
         >:: fun _ ->
           let e1 = create_wall () in
           let w = put_entity empty e1 in
           let all_e = all_entities w in
           assert_equal 1 (List.length all_e);
           match all_e with
           | h :: _ -> assert_equal e1.id h.id
           | _ ->
               assert_bool
                 "entity in list does not match the entity added to the world"
                 false );
         ( "adding a single entity to a world and retrieving it with query_id \
            should return an option containing that entity"
         >:: fun _ ->
           let e1 = create_wall () in
           let w = put_entity empty e1 in
           assert_equal (Some e1) (query_id w e1.id) );
         ( "retrieving an entity from an empty world by id should produce None"
         >:: fun _ ->
           let e1 = create_wall () in
           let w = empty in
           assert_equal None (query_id w e1.id) );
         ( "retrieving an entity added to position (0, 0) should return that \
            entity"
         >:: fun _ ->
           let e1 = create_wall () in
           let w = put_entity empty e1 in
           assert_equal (Some e1) (query_pos w (0, 0)) );
         ( "querying position (0, 0) in an empty world should return None"
         >:: fun _ ->
           let w = empty in
           assert_equal None (query_pos w (0, 0)) );
         ( "adding a single entity then updating it with put_entity should \
            return a world that still contains that entity"
         >:: fun _ ->
           let e1 = create_wall () in
           let w1 = put_entity empty e1 in
           let w2 = put_entity w1 e1 in
           assert_equal (Some e1) (query_id w2 e1.id) );
         ( "adding a single entity then updating it with put_entity of the \
            same entity moved to (1, 0) should return a world that still \
            contains that entity"
         >:: fun _ ->
           let e1 = create_wall () in
           let w1 = put_entity empty e1 in
           let w2 = put_entity w1 (set_pos e1 (1, 0)) in
           assert_equal
             (Some (set_pos e1 (1, 0)))
             (query_id w2 e1.id) ~printer:string_of_entity_option );
         ( "adding a single entity then removing it will result in an empty \
            world"
         >:: fun _ ->
           let e1 = create_wall () in
           let w1 = put_entity empty e1 in
           assert_equal (Some e1) (query_id w1 e1.id);
           assert_equal empty (remove_entity w1 e1.id) );
       ]

let _ =
  run_test_tt_main entity_tests;
  run_test_tt_main world_tests
