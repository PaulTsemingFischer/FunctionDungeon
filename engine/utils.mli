type vec2 = int * int
(**[vec2] describes the location of an entity in 2D cartesian space*)

val add_vec2 : vec2 -> vec2 -> vec2
(**[add_vec2 vec1 vec2] is the sum of two positions*)

val neg_vec2 : vec2 -> vec2
(**[neg_vec2 vec] is the negated version of [vec]*)

val sub_vec2 : vec2 -> vec2 -> vec2
(**[sub_vec2 vec1] is [(fst vec1 - fst vec2, snd vec1 - snd vec2)]*)

val get_at_vec : 'a array array -> vec2 -> 'a
(**[get_at_vec matrix vec] is the element in [matrix] at location [vec]. Raises
   [Invalid_argument "index out of bounds"] if [vec2] does not represent a valid
   location in [matrix]. *)

val get_at_vec_opt : 'a array array -> vec2 -> 'a option
(**[get_at_vec_opt matrix vec] is an optional element in [matrix] at location
   [vec].*)

val set_at_vec : 'a array array -> vec2 -> 'a -> unit
(**[set_at_vec matrix vec x] sets the location [vec] in [matrix] to [x]. Raises
   [Invalid_argument "index out of bounds"] if [vec2] does not represent a valid
   location in [matrix]. *)

val neighbors : vec2 -> vec2 list
(**[neighbors vec] is the list of the 4 vec2s adjacent to [vec] *)

val random_dir : unit -> vec2
(**[random_dir] is a random vec2 offset. *)

val string_of_vec : vec2 -> string
(**[string_of_vec vec] is a string representation of [vec]*)
