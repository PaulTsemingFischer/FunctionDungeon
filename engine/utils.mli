type vec2 = int * int
(**[vec2] describes the location of an entity in 2D cartesian space*)

val add_vec2 : vec2 -> vec2 -> vec2
(**[add_vec2 vec1 vec2] is the sum of two positions*)

val neg_vec2 : vec2 -> vec2
(**[neg_vec2 vec] is the negated version of [vec]*)

val sub_vec2 : vec2 -> vec2 -> vec2
(**[sub_vec2 vec1] is [(fst vec1 - fst vec2, snd vec1 - snd vec2)]*)

val mul_vec2 : vec2 -> vec2 -> vec2
(**[mul_vec2 (x1, y1) (x2, y2)] is [(x1 * x2, y1 * y2)]*)


val scale_vec2 : vec2 -> int -> vec2
(**[scale_vec2 vec1 s] is [(fst vec1 * s, snd vec1 * s)]*)

val string_of_vec2 : vec2 -> string
(**[string_of_vec2 vec] is a string representation of [vec]*)

type vec2f = float * float
(**[vec2f] describes the location of an entity in 2D cartesian space, but as a float*)

val add_vec2f : vec2f -> vec2f -> vec2f
(**[add_vec2f vec1 vec2] is the sum of two positions*)

val neg_vec2f : vec2f -> vec2f
(**[neg_vec2f vec] is the negated version of [vec]*)

val sub_vec2f : vec2f -> vec2f -> vec2f
(**[sub_vec2f vec1] is [(fst vec1 - fst vec2, snd vec1 - snd vec2)]*)

val mul_vec2f : vec2f -> vec2f -> vec2f
(**[mul_vec2f (x1, y1) (x2, y2)] is [(x1 *. x2, y1 *. y2)]*)

val scale_vec2f : vec2f -> float -> vec2f
(**[scale_vec2f vec1 s] is [(fst vec1 *. s, snd vec1 *. s)]*)

val round_vec2f : vec2f -> vec2f
(**[round_vec2f vec1] rounds the x and y components of [vec1] to the nearest whole number*)

val vec2_of_vec2f: vec2f -> vec2
(**[vec2_of_vec2f vec1] floors the x and y components of [vec1] and returns a [vec2]*)

val vec2f_of_vec2: vec2 -> vec2f
(**[vec2f_of_vec2 vec1] converts the components of [vec1] to floats*)



val string_of_vec2f : vec2f -> string
(**[string_of_vec2f vec] is a string representation of [vec]*)

val lerp : float -> float -> float -> float
(**[lerp f1 f2 t] returns the linear interpolation of [f2] to [f1] by [t * (f1 - f2)]*)

val lerp_vec : vec2f -> vec2f -> float -> vec2f
(**[lerp_vec vf1 vf2 t] returns the linear interpolation of [vf2] to [vf1] by [t * (vf1 - vf2)]*)

val length_squared : vec2f -> float
(**[length_squared vf] returns the square of the magnitude of [vf]*)

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

