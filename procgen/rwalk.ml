open Engine.Utils
open BatList
open Seq

type tile =
  | Void
  | Ground

type t = tile array array

let string_of_tile = function
  | Void -> "·"
  | Ground -> "☐"

let string_of_genworld (arr : t) =
  String.concat "\n"
    (Array.map
       (fun row ->
         String.concat " " (Array.map string_of_tile row |> Array.to_list))
       arr
    |> Array.to_list)
  ^ "\n"

(**[generate_aux world walkers nwalkers] walks a random walker from the list of [walkers]
   1 step with no more than [nwalkers] existing at any moment. *)
let generate_aux world walkers nwalkers =
  let random_walker () =
    BatList.at !walkers (Random.int (BatList.length !walkers)) in
  try
    let walker = random_walker () in
    for i = 1 to 4 do
      let tile = add_vec2 !walker (random_dir ()) in
      if get_at_vec_opt world tile = Some Void then begin
        set_at_vec world tile Ground;
        walker := tile;

        if BatList.length !walkers < nwalkers then
          walkers := BatList.cons (random_walker ()) !walkers;
        (*Add walker*)
        raise Exit
      end
    done;
    walkers := BatList.remove !walkers walker (*Kill lost walker *)
  with Exit -> ()

(**[post_process world] is the world after being processed to avoid narrow passages. *)
let post_process world fill_chance =
  Array.mapi
    (fun outer_index arr ->
      Array.mapi
        (fun inner_index tile ->
          if Random.float 1.0 > fill_chance then tile
          else
            let neighbor_tiles =
              List.map (get_at_vec_opt world)
                (neighbors (outer_index, inner_index))
              |> List.filter Option.is_some |> List.map Option.get
            in
            if List.mem Ground neighbor_tiles then Ground else tile)
        arr)
    world

let generate ?(printing = false) ?(nwalkers = 5) ?(pp_fill_chance = 0.4) width height =
  Random.self_init ();
  let world = Array.init height (fun _ -> Array.make width Void) in
  let center : vec2 = (height / 2, width / 2) in
  set_at_vec world center Ground;
  let walkers = ref [ ref center ] in
  while BatList.length !walkers <> 0 do
    if printing then string_of_genworld world |> print_endline;
    generate_aux world walkers nwalkers
  done;
  if printing then string_of_genworld world |> print_endline;
  let world = post_process world pp_fill_chance in
  if printing then string_of_genworld world |> print_endline;
  world
