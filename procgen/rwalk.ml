open Engine.Utils
open BatList
open Seq

type tile =
  | Void
  | Ground

type t = tile array array

(*Tuning numbers *)
let num_walkers = 10
let percent_post_processing_filled = 0.4

(**[string_of_tile t] is a string representation of the tile [t] *)
let string_of_tile = function
  | Void -> "·"
  | Ground -> "☐"

(**[string_of_genworld w] is a string representation of the world [w] *)
let string_of_genworld (arr : t) =
  String.concat "\n"
    (Array.map
       (fun row ->
         String.concat " " (Array.map string_of_tile row |> Array.to_list))
       arr
    |> Array.to_list)
  ^ "\n"

let random_walker walkers =
  BatList.at !walkers (Random.int (BatList.length !walkers))

(**[generate_aux world walkers] walks a random walker from the list of [walkers]
   1 step. *)
let generate_aux world walkers =
  try
    let walker = random_walker walkers in
    for i = 1 to 4 do
      let tile = add_vec2 !walker (random_dir ()) in
      if get_at_vec_opt world tile = Some Void then begin
        set_at_vec world tile Ground;
        walker := tile;

        if BatList.length !walkers < num_walkers then
          walkers := BatList.cons (random_walker walkers) !walkers;
        (*Add walker*)
        raise Exit
      end
    done;
    walkers := BatList.remove !walkers walker (*Kill lost walker *)
  with Exit -> ()

(**[post_process world] is the world after being processed. *)
let post_process world =
  Array.mapi
    (fun outer_index arr ->
      Array.mapi
        (fun inner_index tile ->
          if Random.float 1.0 > percent_post_processing_filled then tile
          else
            let neighbor_tiles =
              List.map (get_at_vec_opt world)
                (neighbors (outer_index, inner_index))
              |> List.filter Option.is_some |> List.map Option.get
            in
            if List.mem Ground neighbor_tiles then Ground else tile)
        arr)
    world

(**[generate ~printing width height] is random-walk assignment of tiles to a
   space of size [width] x [height]. IF [printing], each stage of the world
   generation is printed. *)
let generate ?(printing = false) width height =
  Random.self_init ();
  (*world is a columm order matrix representing the world*)
  let world = Array.init height (fun _ -> Array.make width Void) in
  let center : vec2 = (height / 2, width / 2) in
  set_at_vec world center Ground;
  let walkers = ref [ ref center; ref center; ref center; ref center ] in
  while BatList.length !walkers <> 0 do
    if printing then string_of_genworld world |> print_endline;
    generate_aux world walkers
  done;
  if printing then string_of_genworld world |> print_endline;
  let world = post_process world in
  if printing then string_of_genworld world |> print_endline;
  world
