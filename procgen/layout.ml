open Queue
open Engine.Utils
type t = bool array array

let gen_layout n = 
  let side_length =  4 * int_of_float (ceil (sqrt (float_of_int n))) in
  let layout = Array.make side_length (Array.make side_length false) in
  let frontier = ref [] in
  frontier := ((side_length/2), (side_length/2)) :: !frontier;
  for i = 1 to n do 
    let rand_index = Random.int (List.length !frontier) in
    let spot = List.nth !frontier rand_index in
    frontier := List.mapi (fun i x -> if i = rand_index then None else Some x) !frontier |> List.filter_map Fun.id;
    set_at_vec layout spot true;
    let false_neighbors = cardinal_neighbors spot |> List.filter (fun x -> get_at_vec_opt layout x = Some false) in
    List.iter (fun x -> frontier := x :: !frontier) false_neighbors;
  done;
  layout




