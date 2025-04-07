open OUnit2
open Engine
open Engine.Utils

type test_stat = { health : float }
type test_types = TestEntity
type test_status_effects = Generic

module BaseTestDeclarations :
  Entity.EntityData
    with type t = test_stat
     and type entity_type = test_types
     and type status_effect = test_status_effects = struct
  type t = test_stat
  type entity_type = test_types
  type status_effect = test_status_effects

  let zeroed_stats = { health = 0.0 }
  let string_of_stats stat = Printf.sprintf "health: %f" stat.health
  let string_of_type e_type = "test"
  let string_of_status (_ : status_effect) = "generic"
end

module TestEntity = Entity.Make (BaseTestDeclarations)
module TestWorld = World.Make (TestEntity)

let string_of_test_status (e_status : TestEntity.status_effect) =
  match e_status with
  | _ -> failwith "test error: unsupported entity status"

(**[create_test_entity ()] is utility method that creates a test_entity entity*)
let create_test_entity () =
  TestEntity.create { health = 0. } TestEntity [] (0, 0)

(**[string_of_entity_option op] converts [op] into a string*)
let string_of_entity_option (op : TestEntity.t option) =
  match op with
  | Some x -> TestEntity.string_of_entity x
  | None -> "none"

(**[utils_tests] tests operations with vec2s*)
let utils_tests =
  "test suite that tests vec2 operations"
  >::: [
         ( "add_vec2 correctly computes the sum of two vectors" >:: fun _ ->
           assert_equal (1, 2) (add_vec2 (0, 1) (1, 1)) );
         ( "neg_vec2 correctly computes the negation of a vector" >:: fun _ ->
           assert_equal (-3, -1) (neg_vec2 (3, 1)) );
         ( "neg_vec2 correctly computes the difference between two vectors"
         >:: fun _ -> assert_equal (-1, 0) (sub_vec2 (0, 1) (1, 1)) );
         ( "string_of_vec2 correctly converts the given vec2 to a string"
         >:: fun _ -> assert_equal "(-1,0)" (string_of_vec2 (-1, 0)) );
         ( "add_vec2f correctly computes the sum of two vectors" >:: fun _ ->
           assert_equal (1., 2.) (add_vec2f (0., 1.) (1., 1.)) );
         ( "neg_vec2f correctly computes the negation of a vector" >:: fun _ ->
           assert_equal (-3., -1.) (neg_vec2f (3., 1.)) );
         ( "neg_vec2f correctly computes the difference between two vectors"
         >:: fun _ -> assert_equal (-1., 0.) (sub_vec2f (0., 1.) (1., 1.)) );
         ( "string_of_vec22f correctly converts the given vec2 to a string"
         >:: fun _ ->
           assert_equal "(-1.,0.)" (string_of_vec2f (-1., 0.)) ~printer:Fun.id
         );
         ( "lerp of 0 to 1 by a threshold of 0.5 should equal 0.5" >:: fun _ ->
           assert_equal 0.5 (lerp 0. 1. 0.5) );
         ( "lerp of 0 to 2 by a threshold of 0.5 should equal 1.0" >:: fun _ ->
           assert_equal 1. (lerp 0. 2. 0.5) );
         ( "lerp_vec of (0, 0) to (1, 1) by a threshold of 0.5 should equal \
            (0.5, 0.5)"
         >:: fun _ -> assert_equal (0.5, 0.5) (lerp_vec (0., 0.) (1., 1.) 0.5)
         );
         ( "lerp_vec of (0, 0) to (0, 1) by a threshold of 0.5 should equal \
            (0., 0.5)"
         >:: fun _ -> assert_equal (0., 0.5) (lerp_vec (0., 0.) (0., 1.) 0.5) );
         ( "lerp_vec of (0, 0) to (1, 0) by a threshold of 0.5 should equal \
            (0.5, 0.)"
         >:: fun _ -> assert_equal (0.5, 0.) (lerp_vec (0., 0.) (1., 0.) 0.5) );
         ( "the length squared of (2., 2.) should be 8." >:: fun _ ->
           assert_equal 8. (length_squared (2., 2.)) );
       ]

(**[entity_tests] tests functionality related to creating entities*)
let entity_tests =
  "test suite that tests basic operations of the Entity module"
  >::: [
         ( "Entity.create produces an entity with id 0 and the provided entity \
            information when no other entities have been created"
         >:: fun _ ->
           let new_entity = create_test_entity () in
           assert_equal { health = 0. } new_entity.stats;
           assert_equal TestEntity new_entity.TestEntity.entity_type;
           assert_equal [] new_entity.statuses );
         ( "Entity.create produces an entity with a different id after one \
            entity is already created"
         >:: fun _ ->
           let e_1 = create_test_entity () and e_2 = create_test_entity () in
           assert_equal { health = 0. } e_2.stats;
           assert_equal TestEntity e_2.TestEntity.entity_type;
           assert_equal [] e_2.statuses;
           assert_bool "entity id should be different, but is the same"
             (not (e_1.id = e_2.id)) );
         ( "Entity.update stats produces an entity with the given stats from \
            while all other attributes copied from a source entity"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let e2 =
             TestEntity.update_stats e1 { health = e1.stats.health -. 1.0 }
           in
           assert_bool "entity id should be the same, but is different"
             (e1.id = e2.id);

           assert_equal { health = -1.0 } e2.stats;
           assert_equal e1.TestEntity.entity_type e2.TestEntity.entity_type;
           assert_equal e1.statuses e2.statuses );
       ]

(**[world_tests] tests functionality related to creating worlds, adding entities
   into worlds, updating worlds, and removing entities*)
let world_tests =
  "test suite that tests creating worlds, adding entities into worlds, \
   updating entities, and removing entities"
  >::: [
         ( "creating an empty world and retrieving all entities in the world \
            returns an empty list of entities"
         >:: fun _ -> assert_equal [] (TestWorld.all_entities TestWorld.empty)
         );
         ( "adding a single entity to a world and retrieving all entities \
            should return a list containing only that entity"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let w = TestWorld.put_entity TestWorld.empty e1 in
           let all_e = TestWorld.all_entities w in
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
           let e1 = create_test_entity () in
           let w = TestWorld.put_entity TestWorld.empty e1 in
           assert_equal (Some e1) (TestWorld.query_id w e1.id) );
         ( "adding two entities to a world and retrieving them in added order \
            with query_id should return an option containing that entity"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let e2 = create_test_entity () in
           let w = TestWorld.put_entity TestWorld.empty e1 in
           let w2 = TestWorld.put_entity w e2 in
           assert_equal (Some e1) (TestWorld.query_id w2 e1.id)
             ~printer:(fun x ->
               match x with
               | Some _ -> "some"
               | None -> "none");
           assert_equal (Some e2) (TestWorld.query_id w2 e2.id)
             ~printer:(fun x ->
               match x with
               | Some _ -> "some"
               | None -> "none") );
         ( "adding two entities in reverse order to a world and retrieving \
            them in added order with query_id should return an option \
            containing that entity"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let e2 = create_test_entity () in
           let w = TestWorld.put_entity TestWorld.empty e2 in
           let w2 = TestWorld.put_entity w e1 in
           assert_equal (Some e2) (TestWorld.query_id w2 e2.id);
           assert_equal (Some e1) (TestWorld.query_id w2 e1.id) );
         ( "adding two entities in normal order to a world and retrieving \
            them  in reverse order with query_id should return an option \
            containing  that entity"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let e2 = create_test_entity () in
           let w = TestWorld.put_entity TestWorld.empty e1 in
           let w2 = TestWorld.put_entity w e2 in
           assert_equal (Some e2) (TestWorld.query_id w2 e2.id);
           assert_equal (Some e1) (TestWorld.query_id w2 e1.id) );
         ( "adding two entities in reverse order to a world and retrieving \
            them in reverse order with query_id should return an option \
            containing that entity"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let e2 = create_test_entity () in
           let w = TestWorld.put_entity TestWorld.empty e2 in
           let w2 = TestWorld.put_entity w e1 in
           assert_equal (Some e1) (TestWorld.query_id w2 e1.id);
           assert_equal (Some e2) (TestWorld.query_id w2 e2.id) );
         ( "retrieving an entity from an empty world by id should produce None"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let w = TestWorld.empty in
           assert_equal None (TestWorld.query_id w e1.id) );
         ( "retrieving an entity added to position (0, 0) should return that \
            entity"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let w = TestWorld.put_entity TestWorld.empty e1 in
           assert_equal (Some e1) (TestWorld.query_pos w (0, 0)) );
         ( "querying position (0, 0) in an empty world should return None"
         >:: fun _ ->
           let w = TestWorld.empty in
           assert_equal None (TestWorld.query_pos w (0, 0)) );
         ( "querying if something is empty (0, 0) in an empty world should \
            return true"
         >:: fun _ ->
           let w = TestWorld.empty in
           assert_bool "the position is supposed to be empty, but is not"
             (not (TestWorld.mem_pos w (0, 0))) );
         ( "querying if something is empty (0, 0) in a world with an entity at \
            (0, 0) should return false"
         >:: fun _ ->
           let w =
             TestWorld.put_entity TestWorld.empty (create_test_entity ())
           in
           assert_bool "the position is not supposed to be empty, but is"
             (TestWorld.mem_pos w (0, 0)) );
         ( "adding a single entity then updating it with put_entity should \
            return a world that still contains that entity"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let w1 = TestWorld.put_entity TestWorld.empty e1 in
           let w2 = TestWorld.put_entity w1 e1 in
           assert_equal (Some e1) (TestWorld.query_id w2 e1.id) );
         ( "adding a single entity then updating it with put_entity of the \
            same entity moved to (1, 0) should return a world that still \
            contains that entity"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let w1 = TestWorld.put_entity TestWorld.empty e1 in
           let w2 = TestWorld.put_entity w1 (TestEntity.set_pos e1 (1, 0)) in
           assert_equal
             (Some (TestEntity.set_pos e1 (1, 0)))
             (TestWorld.query_id w2 e1.id)
             ~printer:string_of_entity_option );
         ( "adding a single entity then removing it will result in an empty \
            world"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let w1 = TestWorld.put_entity TestWorld.empty e1 in
           assert_equal (Some e1) (TestWorld.query_id w1 e1.id);
           assert_equal TestWorld.empty (TestWorld.remove_entity w1 e1.id) );
       ]

let _ =
  run_test_tt_main utils_tests;
  run_test_tt_main entity_tests;
  run_test_tt_main world_tests
