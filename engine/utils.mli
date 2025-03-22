type vec2 = int * int
(**[vec2] describes the location of an entity in 2D cartesian space*)

val add_vec2 : vec2 -> vec2 -> vec2
(**[add_vec2 vec1 vec2] returns the sum of two positions*)

val neg_vec2 : vec2 -> vec2
(**[neg_vec2 vec] returns the negated version of [vec]*)

val sub_vec2 : vec2 -> vec2 -> vec2
(**[sub_vec2 vec1] returns [(fst vec1 - fst vec2, snd vec1 - snd vec2)]*)

val string_of_vec : vec2 -> string
(**[string_of_vec vec] converts [vec] into a string form*)
