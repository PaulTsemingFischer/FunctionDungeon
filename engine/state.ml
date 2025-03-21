type event = ..

type transition_generator = Generator of (t -> Entity.t -> int -> transition)
and t = World.t * transition_generator list * event list
and transition = int * (t -> t)

let create (world : World.t) (generators : transition_generator list) : t =
  (world, generators, [])

type input = ..

exception Invalid_input of input

let step (state : t) (input : input) = state
