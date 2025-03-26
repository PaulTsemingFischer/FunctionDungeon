module type S = sig
  
   type w_t
   type e_t

   (**[input] represents all possible user inputs that can be inputted into program*)
   type input = ..

   (**[event] represents some basic action that has occurred in a world 
      transformation.  Examples: [Move of e_t * (int * int) * (int * int)] 
      can be used to describe a basic movement from one position to another*)
   type event = ..

   and t = {world: w_t; transitions: transition list; events:event list; turn:int}
   (**[t] is the game state that contains a world, a list of transition
      generators, and history of events that describe how the world changes*)

   and transition = (t -> e_t -> input -> t)
   (**[transition] is a prioritized function that maps one world state onto another
      (in other words, it changes the world in some way). transitions are run in increasing priority order*)

   (**[create world generators] returns a new game state with an empty event record and the given list of transition generators*)
   val create:  w_t -> transition list -> t

   (**[Invalid_input] is raised in [step] when an given input produces no valid action*)
   exception Invalid_input of input

   (**[step state input] returns the next state given the the current state [state] and the user input [input]. Increments [t.turn] by 1.
      Raises: [Invalid_input] when the provided input requests an action that is not possible*)
   val step : t -> input -> t

   (**[update_world state new_world] is a utility function that only replaces the world of [state] with [new_world]*)
   val update_world : t ->  w_t -> t
end

module Make (W: World.S) : S with type w_t = W.t and type e_t = W.e_t