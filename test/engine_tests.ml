open OUnit2
open Engine
open Engine.Utils
open Game

type test_stat = { health : float }

module BaseTestStat : Entity.StatType with type t = test_stat = struct
  type t = test_stat

  let zeroed_stats = { health = 0.0 }
  let string_of_stats stat = Printf.sprintf "health: %f" stat.health
end

module TestEntity = Entity.Make (BaseTestStat)
module TestWorld = World.Make (TestEntity)
module TestState = State.Make (TestWorld)

type TestEntity.entity_type += Test_type
type TestEntity.rendering += Test_rendering
type TestState.input += Test_input

let string_of_test_types (e_type : TestEntity.entity_type) =
  match e_type with
  | Test_type -> "test_type"
  | _ -> failwith "test error: unsupported entity type"

let string_of_test_rendering (e_rendering : TestEntity.rendering) =
  match e_rendering with
  | Test_rendering -> "test_rendering"
  | _ -> failwith "test error: unsupported rendering type"

let string_of_test_status (e_status : TestEntity.status) =
  match e_status with
  | _ -> failwith "test error: unsupported entity status"

let string_of_test_entity =
  TestEntity.string_of_entity string_of_test_types string_of_test_rendering
    string_of_test_status

(**[create_test_entity ()] is utility method that creates a test_entity entity*)
let create_test_entity () =
  TestEntity.create { health = 0. } Test_type Test_rendering [] (0, 0)

(**[string_of_entity_option op] converts [op] into a string*)
let string_of_entity_option (op : TestEntity.t option) =
  match op with
  | Some x -> string_of_test_entity x
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
         >:: fun _ -> assert_equal "(-1,0)" (string_of_vec (-1, 0)) );
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
           assert_equal Test_type new_entity.TestEntity.entity_type;
           assert_equal Test_rendering new_entity.rendering;
           assert_equal [] new_entity.statuses );
         ( "Entity.create produces an entity with a different id after one \
            entity is already created"
         >:: fun _ ->
           let e_1 = create_test_entity () and e_2 = create_test_entity () in
           assert_equal { health = 0. } e_2.stats;
           assert_equal Test_type e_2.TestEntity.entity_type;
           assert_equal Test_rendering e_2.rendering;
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
           assert_equal e1.rendering e2.rendering;
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

let simple_transition : TestState.transition =
 fun (start_state : TestState.t) (entity : TestEntity.t)
     (_ : TestState.input) ->
  let updated =
    TestEntity.update_stats entity { health = entity.stats.health -. 0.1 }
  in
  TestState.update_world start_state
    (TestWorld.put_entity start_state.world updated)

let useless_transition (start_state : TestState.t) _ _ : TestState.t =
  start_state

let print_all_entities (w : TestWorld.t) =
  List.iter
    (fun e -> print_endline (string_of_test_entity e))
    (TestWorld.all_entities w)

(**[state_tests] tests stepping through state with multiple state generator
   functions and ensuring that the state at each step is the expected state*)
let state_tests =
  "test suite that tests end-to-end functionality of state progression"
  >::: [
         ( "stepping through the state of a game with no generators and a \
            single entity advances the turn but leaves the entity unchanged"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let world = TestWorld.put_entity TestWorld.empty e1 in
           let state_start = TestState.create world [] in
           assert_equal (Some e1) (TestWorld.query_id state_start.world e1.id);
           assert_equal 0 state_start.turn;
           let state_next = TestState.step state_start Test_input in
           assert_equal (Some e1) (TestWorld.query_id state_next.world e1.id);
           assert_equal 1 state_next.turn );
         ( "stepping through the state of a game with one generator that \
            decreases health and a single entity advances the turn and \
            decreases the entity's health"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let world = TestWorld.put_entity TestWorld.empty e1 in
           let state_start = TestState.create world [ simple_transition ] in
           print_all_entities state_start.world;
           assert_equal (Some e1) (TestWorld.query_id state_start.world e1.id);
           assert_equal 0 state_start.turn;
           let state_next = TestState.step state_start Test_input in
           print_all_entities state_next.world;

           assert_equal
             (Some
                (TestEntity.update_stats e1 { health = e1.stats.health -. 1.0 }))
             (TestWorld.query_id state_next.world e1.id)
             ~printer:string_of_entity_option;
           assert_equal 1 state_next.turn );
         ( "stepping through the state of a game with one generator that does \
            nothing and a single entity advances the turn but leaves the \
            entity unchanged"
         >:: fun _ ->
           let e1 = create_test_entity () in
           let world = TestWorld.put_entity TestWorld.empty e1 in
           let state_start = TestState.create world [ useless_transition ] in
           print_all_entities state_start.world;
           assert_equal (Some e1) (TestWorld.query_id state_start.world e1.id);
           assert_equal 0 state_start.turn;
           let state_next = TestState.step state_start Test_input in
           print_all_entities state_next.world;
           assert_equal (Some e1)
             (TestWorld.query_id state_next.world e1.id)
             ~printer:string_of_entity_option;
           assert_equal 1 state_next.turn );
       ]

module AttackMap = Map.Make (ComparableVec2.Vec2)
(** Represents a map of tile coordinates to attack effects. *)

(** [make_modify_test name expected_output func input] is a unit test with name
    [name], asserting whether [func input] is equal to [expected_output]. *)
let make_modify_test name expected_output func input =
  name >:: fun _ ->
  let output = Item.modify_attack func input in
  assert_equal expected_output output
    ~cmp:(AttackMap.equal (fun a b -> List.equal Item.compare_effects a b))
    ~printer:Item.bindings_to_string

(** [attack_tests] is a series of unit tests ensuring that attack modifiers work
    as expected. *)
let attack_tests =
  "test suite"
  >::: [
         make_modify_test "Add 1 damage to empty"
           AttackMap.(empty |> add (0, 0) [ Item.Damage 1 ])
           Item.do_damage_example
           AttackMap.(empty |> add (0, 0) []);
         make_modify_test "Add 1 damage on top of existing effects"
           AttackMap.(empty |> add (0, 0) [ Item.Fire (2, 3); Item.Damage 1 ])
           Item.do_damage_example
           AttackMap.(empty |> add (0, 0) [ Item.Fire (2, 3) ]);
         make_modify_test "Augment to above tile"
           AttackMap.(empty |> add (0, 0) [] |> add (0, 1) [])
           Item.augment_to_above_example
           AttackMap.(empty |> add (0, 0) []);
         make_modify_test "Augment to above tile with duplicates"
           AttackMap.(empty |> add (0, 0) [] |> add (0, 1) [] |> add (0, 2) [])
           Item.augment_to_above_example
           AttackMap.(empty |> add (0, 0) [] |> add (0, 1) []);
         make_modify_test "Augment to above tile and copy effects"
           AttackMap.(
             empty
             |> add (0, 0) [ Item.Damage 1 ]
             |> add (0, 1) [ Item.Damage 1 ])
           Item.augment_to_above_example
           AttackMap.(empty |> add (0, 0) [ Item.Damage 1 ]);
         make_modify_test "Augment to above tile and add new effects"
           AttackMap.(
             empty
             |> add (0, 0) [ Item.Damage 1 ]
             |> add (0, 1) [ Item.Damage 1; Item.Damage 1 ]
             |> add (0, 2) [ Item.Damage 1 ])
           Item.augment_to_above_example
           AttackMap.(
             empty
             |> add (0, 0) [ Item.Damage 1 ]
             |> add (0, 1) [ Item.Damage 1 ]);
       ]

let _ =
  (* run_test_tt_main utils_tests; run_test_tt_main entity_tests;
     run_test_tt_main world_tests; run_test_tt_main state_tests; *)
  run_test_tt_main attack_tests
