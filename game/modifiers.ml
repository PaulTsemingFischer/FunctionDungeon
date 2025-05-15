open Engine.Utils

type action =
  | DealDamage of float
  | ApplyFire of float * int
  | DealFireDamage of float
  | BarrierAttack of int * Obstacles.obstacle
  | StealAttack
  | FogAttack of int * int

type possible_action = vec2 * action list
type possible_move = vec2

type possible_actions_modifier =
  | ScaleAction of int
  | AddFire of float * int
  | AddDamage of float
  | AugmentToAdjacent

type possible_moves_modifier = ScaleMove of int

let base_cross_moves : possible_move list = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

let rec range_cross_moves (r : int) : possible_move list =
  match r with
  | 0 -> []
  | n1 ->
      let rec n2_loop n2 acc =
        if n2 < 0 then acc
        else
          let new_moves = [ (n1, n2); (-n1, n2); (n1, -n2); (-n1, -n2) ] in
          n2_loop (n2 - 1) (new_moves @ acc)
      in
      let moves_for_n1 = n2_loop n1 [] in
      moves_for_n1 @ range_cross_moves (n1 - 1)

let base_cross_actions : possible_action list =
  List.map (fun target -> (target, [ DealDamage 1. ])) base_cross_moves

let range_cross_actions r : possible_action list =
  List.map (fun target -> (target, [ DealDamage 1. ])) (range_cross_moves r)

let var_damage_cross_actions d : possible_action list =
  List.map (fun target -> (target, [ DealDamage d ])) base_cross_moves

let var_range_damage_cross_actions r d : possible_action list =
  List.map (fun target -> (target, [ DealDamage d ])) (range_cross_moves r)

let enemy_attack_type (e : Enemytype.enemy) : action =
  match e with
  | Jailer (r, t) -> BarrierAttack (r, Obstacles.Fence t)
  | Thief -> StealAttack
  | Fog_Cloud (r, f) -> FogAttack (r, f)
  | Variable_Range r -> DealDamage 1.
  | Variable_Damage d -> DealDamage d
  | Variable_Range_and_Damage (r, d) -> DealDamage d

let enemy_cross_actions (e : Enemytype.enemy) : possible_action list =
  match e with
  | Variable_Range r -> range_cross_actions r
  | Variable_Damage d -> var_damage_cross_actions d
  | Variable_Range_and_Damage (r, d) -> var_range_damage_cross_actions r d
  | _ ->
      List.map
        (fun target -> (target, [ enemy_attack_type e ]))
        base_cross_moves

[@@@coverage off]

let string_of_modifier m =
  match m with
  | ScaleAction x -> Printf.sprintf "buff: scale attack range by %d" x
  | AddFire (x, y) ->
      Printf.sprintf "buff: apply %0.2f fire damage for %d turns" x y
  | AddDamage x -> Printf.sprintf "buff: deal %0.2f extra damage" x
  | AugmentToAdjacent -> "buff: increase attack area"

[@@@coverage on]
