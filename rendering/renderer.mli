open Game

type t
(**[t] represents the renderer's type*)

type input_handler = GameState.t -> GameState.input -> GameState.t
(**[input_handler state input] is the signature of a function that handles
   inputs from the GUI*)

val loop : t -> GameState.t -> input_handler -> unit
(**[loop renderer state] starts a loop that scans for input and renders the game
   state onto a window*)

val make_from_state : GameState.t -> t
(**[make_from_state] returns a renderable that reflects the state of [t]*)
