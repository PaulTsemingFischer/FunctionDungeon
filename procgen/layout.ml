open Queue
open Engine.Utils
type t = bool array array

let gen_layout n = 
  let side_length =  4 * int_of_float (ceil (sqrt (float_of_int n))) in
  let layout = Array.make side_length (Array.make side_length false) in
  let frontier = Queue.create () in
  push ((side_length/2), (side_length/2)) frontier;
  for i = 1 to n do 
    let spot: vec2 = pop frontier in
    set_at_vec layout spot true;
    let false_neighbors = cardinal_neighbors spot |> List.filter (fun x -> get_at_vec_opt layout x = Some false) in
    List.iter (fun x -> push x frontier) false_neighbors;
  done;
  layout




