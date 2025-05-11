open Engine.Utils

(**[action] describes an action that some entity in the world can take*)
type action =
  | DealDamage of float
  | ApplyFire of int
  | DealFireDamage
  | BarrierAttack of int * Obstacles.obstacle
  | StealAttack

type possible_action = vec2 * action list
(**[possible_action] is an action associated with a tile*)

type possible_move = vec2
(**[possible_move] is an movement to a tile*)

type possible_actions_modifier =
  | ScaleAction of int
  | AddFire of int
  | AugmentToAdjacent

(**[possible_action_function] is a function that changes a list of action
   modifiers in some way*)

type possible_moves_modifier =
  | ScaleMove of int
      (**[possible_move_function] is a function that changes a list of move
         modifiers in some way*)

(**[base_cross_moves] is a list containing the most basic movement pattern*)
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

(**[base_cross_actions] is a list containing the most basic acting pattern*)
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
  | Blinder t -> exit 0 (* Dummy for now *)
  | Fog_Cloud (r, t) -> exit 0 (* Dummy for now *)
  | Variable_Range r -> DealDamage 1.
  | Variable_Damage d -> DealDamage d
  | Variable_Damage_and_Range (r, d) -> DealDamage d

let enemy_cross_actions (e : Enemytype.enemy) : possible_action list =
  match e with
  | Variable_Range r -> range_cross_actions r
  | Variable_Damage d -> var_damage_cross_actions d
  | Variable_Damage_and_Range (r, d) -> var_range_damage_cross_actions r d
  | _ ->
      List.map
        (fun target -> (target, [ enemy_attack_type e ]))
        base_cross_moves
