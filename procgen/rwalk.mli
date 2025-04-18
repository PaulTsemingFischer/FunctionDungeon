(* open Game.GameDefinitions

type tile =
  | Void
  | Ground

type t = tile array array
(**[t] is a world of tiles stored in column major order so that [t.(w).(h)]
   accesses the (h, w) element.*)

val string_of_tile : tile -> string
(**[string_of_tile t] is a string representation of the tile [t] *)

val string_of_genworld : t -> string
(**[string_of_genworld w] is a string representation of the world [w] *)

val generate :
  ?printing:bool ->
  ?walker_age:int ->
  ?nwalkers:int ->
  ?pp_fill_chance:float ->
  int ->
  int ->
  tile array array
(**[generate ~printing ~walker_age ~nwalkers ~pp_fill_chance width height] is
   random-walk assignment using [nwalkers] walkers of tiles to a space of size
   [width] x [height]. If [printing], each stage of the world generation is
   printed. When [pp_fill_chance] is closer to 1, the resulting world will be
   smoother and when it is closer to 0, the world will be more random and
   disconnected.*)

val world_from_genworld : t -> GameWorld.t
(**[world_from_genworld] is the usable world from the temporary creation world.*)
d *)
