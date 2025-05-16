open OUnit2
open Game
open Game.GameDefinitions

(**[string_of_entity_option op] converts [op] into a string*)
let string_of_entity_option (op : GameEntity.t option) =
  match op with
  | Some x -> GameEntity.string_of_entity x
  | None -> "none"

let simple_transition : GameState.transition =
 fun (start_state : GameState.t) (entity : GameWorld.e_t)
     (_ : GameState.input) ->
  let entity = to_gameentity_type entity in
  let stats = to_entity_stats entity.stats in
  let updated =
    GameEntity.update_stats entity
      (to_gameentity_stats
         {
           health = stats.health -. 1.0;
           base_actions = stats.base_actions;
           base_moves = stats.base_moves;
         })
  in
  GameState.set_room start_state
    (GameWorld.put_entity
       (GameState.room start_state)
       (to_gameworld_type updated))

let useless_transition (start_state : GameState.t) _ _ : GameState.t =
  start_state

let print_all_entities (w : GameWorld.t) =
  List.iter
    (fun e -> print_endline (GameEntity.string_of_entity e))
    (List.map (fun x -> to_gameentity_type x) (GameWorld.all_entities w))

(**[create_wall ()] is utility method that creates a wall*)

let create_wall () = create_default_at Wall (0, 0)

(**[state_tests] tests stepping through state with multiple state generator
   functions and ensuring that the state at each step is the expected state*)
let state_tests =
  "test suite that tests end-to-end functionality of state\n\n   progression"
  >::: [
         ( "stepping through the state of a game with no\n\
           \ generators and a single entity advances the turn but leaves  the \
            entity\n\
           \ unchanged"
         >:: fun _ ->
           let e1 = create_wall () in
           let world =
             GameWorld.put_entity GameWorld.empty (to_gameworld_type e1)
           in
           let player = create_default_at Player (0, 0) in
           let state_start =
             GameState.create [ world ] [] [] (to_gameworld_type player) 0
           in
           assert_equal (Some e1)
             (match
                GameWorld.query_id
                  (GameState.room state_start)
                  (to_entity_id e1.id)
              with
             | None -> None
             | Some x -> Some (to_gameentity_type x));
           assert_equal 0 (GameState.get_turn state_start);
           let state_next = GameState.step state_start Wait in
           assert_equal (Some e1)
             (match
                GameWorld.query_id
                  (GameState.room state_next)
                  (to_entity_id e1.id)
              with
             | None -> None
             | Some x -> Some (to_gameentity_type x));
           assert_equal 1 (GameState.get_turn state_next) );
         ( "stepping\n\
           \ through the state of a game with one generator that decreases  \
            health and\n\
           \   a\n\
           \  single entity advances the turn and decreases the entity's  \
            health"
         >:: fun _ ->
           let e1 = create_wall () in
           let world =
             GameWorld.put_entity GameWorld.empty (to_gameworld_type e1)
           in
           let player = create_default_at Player (0, 0) in
           let state_start =
             GameState.create [ world ] [] [ simple_transition ]
               (to_gameworld_type player) 0
           in
           print_all_entities (GameState.room state_start);
           assert_equal (Some e1)
             (match
                GameWorld.query_id
                  (GameState.room state_start)
                  (to_entity_id e1.id)
              with
             | None -> None
             | Some x -> Some (to_gameentity_type x));
           assert_equal 0 (GameState.get_turn state_start);
           let state_next = GameState.step state_start Wait in
           print_all_entities (GameState.room state_next);

           assert_equal
             (Some
                (GameEntity.update_stats e1
                   (let stats = to_entity_stats e1.stats in
                    to_gameentity_stats
                      {
                        health = stats.health -. 1.0;
                        base_actions = stats.base_actions;
                        base_moves = stats.base_moves;
                      })))
             (match
                GameWorld.query_id
                  (GameState.room state_next)
                  (to_entity_id e1.id)
              with
             | None -> None
             | Some x -> Some (to_gameentity_type x))
             ~printer:string_of_entity_option;
           assert_equal 1 (GameState.get_turn state_next) );
         ( "stepping through the state of a game with one generator that does \
            nothing\n\
           \  and a single entity advances the turn but leaves the entity \
            unchanged"
         >:: fun _ ->
           let e1 = create_wall () in
           let world =
             GameWorld.put_entity GameWorld.empty (to_gameworld_type e1)
           in
           let player = create_default_at Player (0, 0) in
           let state_start =
             GameState.create [ world ] [] [ useless_transition ]
               (to_gameworld_type player) 0
           in
           print_all_entities (GameState.room state_start);
           assert_equal (Some e1)
             (match
                GameWorld.query_id
                  (GameState.room state_start)
                  (to_entity_id e1.id)
              with
             | None -> None
             | Some x -> Some (to_gameentity_type x));
           assert_equal 0 (GameState.get_turn state_start);
           let state_next = GameState.step state_start Wait in
           print_all_entities (GameState.room state_next);
           assert_equal (Some e1)
             (match
                GameWorld.query_id
                  (GameState.room state_next)
                  (to_entity_id e1.id)
              with
             | None -> None
             | Some x -> Some (to_gameentity_type x))
             ~printer:string_of_entity_option;
           assert_equal 1 (GameState.get_turn state_next) );
       ]

let _ = run_test_tt_main state_tests
