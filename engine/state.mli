(**[input] represents all possible user inputs that can be inputted into program*)
type input = ..

(**[event] represents some basic action that has occurred in a world 
   transformation.  Examples: [Move of Entity.t * (int * int) * (int * int)] 
   can be used to describe a basic movement from one position to another*)
type event = ..

(**[transition_generator] is a type that takes the game state, an entity, an
   input, and returns a [transition] that describes how the world should be
   changed*)
type transition_generator = Generator of (t -> Entity.t -> input -> transition option)

and t = World.t * transition_generator list * event list
(**[t] is the game state that contains a world, a list of transition
   generators, and history of events that describe how the world changes*)

and transition = int * (t -> t)
(**[transition] is a prioritized function that maps one world state onto another
   (in other words, it changes the world in some way)*)

(**[create world generators] returns a new game state with an empty event record and the given list of transition generators*)
val create: World.t -> transition_generator list -> t

(**[Invalid_input] is raised in [step] when an given input produces no valid action*)
exception Invalid_input of input

(**[step state input] returns the next state given the the current state [state] and the user input [input].
   Raises: [Invalid_input] when the provided input requests an action that is not possible*)
val step : t -> input -> t