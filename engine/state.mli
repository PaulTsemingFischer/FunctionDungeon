type input = Move of int

(**[event] represents some basic action that has occurred in a world 
   transformation.  Examples: [Move of Entity.t * (int * int) * (int * int)] 
   can be used to describe a basic movement from one position to another*)
type event = ..

(**[transition_generator] is a type that takes the game state, an entity, an
   input, and returns a [transition] that describes how the world should be
   changed*)
type transition_generator = Generator of (t -> Entity.t -> int -> transition)

and t = World.t * transition_generator list * event list
(**[t] is the game state that contains a world, a list of transition
   generators, and history of events that describe how the world changes*)

and transition = int * (t -> t)
(**[transition] is a prioritized function that maps one world state onto another
   (in other words, it changes the world in some way)*)
