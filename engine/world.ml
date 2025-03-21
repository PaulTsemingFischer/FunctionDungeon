type t = []

let empty = []
let query_at world pos = None
let query_id world e_id = None
let all_entities (world : t) : Entity.t list = []
let put_entity (world : t) (_ : Entity.t) = world
