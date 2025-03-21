type input = ..
type event = ..

type transition_generator =
  | Generator of (t -> Entity.t -> input -> transition option)

and t = {
  world : World.t;
  generators : transition_generator list;
  events : event list;
  turn : int;
}

and transition = int * (t -> t)

let create (world : World.t) (generators : transition_generator list) : t =
  { world; generators; events = []; turn = 0 }

exception Invalid_input of input

let step ({ world; generators; events; turn } : t) (input : input) =
  let transitions =
    List.fold_left
      (fun (ext_acc : transition list) (entity : Entity.t) ->
        List.fold_left
          (fun (acc : transition list) (Generator generator_fn) ->
            match
              generator_fn { world; generators; events; turn } entity input
            with
            | Some new_transition -> new_transition :: acc
            | None -> acc)
          ext_acc generators)
      [] (World.all_entities world)
  in
  List.fold_left
    (fun starting_world_state ((_, transition_function) : transition) ->
      transition_function starting_world_state)
    { world; generators; events; turn = turn + 1 }
    transitions

let update_world { world; generators; events; turn } new_world : t =
  { world = new_world; generators; events; turn }
