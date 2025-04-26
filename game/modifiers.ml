open Engine.Utils

(**[action] describes an action that some entity in the world can take*)
type action =
  | DealDamage of float
  | ApplyFire of int
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

(**[base_cross_actions] is a list containing the most basic acting pattern*)
let base_cross_actions : possible_action list =
  List.map (fun target -> (target, DealDamage 1.)) base_cross_moves

let enemy_attack_type (e : Enemytype.enemy) : action =
  match e with
  | Jailer (r, t) -> BarrierAttack (r, Obstacles.Fence t)
  | Thief -> StealAttack
  | Blinder t -> exit 0 (* Dummy for now *)
  | Fog_Cloud (r, t) -> exit 0 (* Dummy for now *)

let enemy_cross_actions e : possible_action list =
  List.map (fun target -> (target, enemy_attack_type e)) base_cross_moves
