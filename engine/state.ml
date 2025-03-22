module type S = sig
  type w_t
  type e_t
  type input = ..
  type event = ..

  type transition_generator =
    | Generator of (t -> e_t -> input -> transition option)

  and t = {
    world : w_t;
    generators : transition_generator list;
    events : event list;
    turn : int;
  }

  and transition = int * (t -> t)

  val create : w_t -> transition_generator list -> t

  exception Invalid_input of input

  val step : t -> input -> t
  val update_world : t -> w_t -> t
end

module Make (W : World.S) : S with type w_t = W.t and type e_t = W.e_t = struct
  type w_t = W.t
  type e_t = W.e_t
  type input = ..
  type event = ..

  type transition_generator =
    | Generator of (t -> e_t -> input -> transition option)

  and t = {
    world : w_t;
    generators : transition_generator list;
    events : event list;
    turn : int;
  }

  and transition = int * (t -> t)

  let create (world : w_t) (generators : transition_generator list) : t =
    { world; generators; events = []; turn = 0 }

  exception Invalid_input of input

  let step ({ world; generators; events; turn } : t) (input : input) =
    let transitions =
      List.fold_left
        (fun (ext_acc : transition list) (entity : e_t) ->
          List.fold_left
            (fun (acc : transition list) (Generator generator_fn) ->
              match
                generator_fn { world; generators; events; turn } entity input
              with
              | Some new_transition -> new_transition :: acc
              | None -> acc)
            ext_acc generators)
        [] (W.all_entities world)
    in
    List.fold_left
      (fun starting_world_state ((_, transition_function) : transition) ->
        transition_function starting_world_state)
      { world; generators; events; turn = turn + 1 }
      transitions

  let update_world { world; generators; events; turn } new_world : t =
    { world = new_world; generators; events; turn }
end
