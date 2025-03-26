module type S = sig
  type w_t
  type e_t
  type input = ..

  type event = ..

  and t = {
    world : w_t;
    transitions : transition list;
    events : event list;
    turn : int;
  }

  and transition = t -> e_t -> input -> t

  val create : w_t -> transition list -> t

  exception Invalid_input of input

  val step : t -> input -> t
  val update_world : t -> w_t -> t
end

module Make (W : World.S) : S with type w_t = W.t and type e_t = W.e_t = struct
  type w_t = W.t
  type e_t = W.e_t
  type input = ..
  type event = ..

  type t = {
    world : w_t;
    transitions : transition list;
    events : event list;
    turn : int;
  }

  and transition = t -> e_t -> input -> t

  let create (world : w_t) (transitions : transition list) : t =
    { world; transitions; events = []; turn = 0 }

  exception Invalid_input of input

  let step (state : t) (input : input) =
    List.fold_left
      (fun (state_ext : t) (entity : e_t) ->
        List.fold_left
          (fun (acc : t) (transition : transition) ->
            transition state_ext entity input)
          state_ext state_ext.transitions)
      state
      (W.all_entities state.world)

  let update_world { world; transitions; events; turn } new_world : t =
    { world = new_world; transitions; events; turn }
end
