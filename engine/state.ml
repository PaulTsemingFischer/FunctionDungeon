type input = Move of int
type event = ..

type transition_generator = Generator of (t -> Entity.t -> int -> transition)
and t = World.t * transition_generator list
and transition = int * (t -> t)
