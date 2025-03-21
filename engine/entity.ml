type id = int
type stats = { health : float }

type entity_type =
  | Player
  | Wall

type rendering = Ascii of char

type status =
  | Poisoned of int
  | Invisible of int

type t = {
  id : int;
  stats : stats;
  entity_type : entity_type;
  rendering : rendering;
  statuses : status list;
}

(*a ref containing the last id assigned to a entity*)
let next_available_id = ref 0

(*a function that generates the next available id*)
let gen_next_id () =
  let current_id = !next_available_id in
  incr next_available_id;
  current_id

let create e_stats e_type e_rendering e_statuses =
  {
    id = gen_next_id ();
    stats = e_stats;
    entity_type = e_type;
    rendering = e_rendering;
    statuses = e_statuses;
  }
