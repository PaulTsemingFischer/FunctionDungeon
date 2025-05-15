open Engine.Utils

(** [action] describes an action that some entity in the world can take*)
type action =
  | DealDamage of float
  | ApplyFire of float * int
  | DealFireDamage of float
  | BarrierAttack of int * Obstacles.obstacle
  | StealAttack
  | FogAttack of int * int

type possible_action = vec2 * action list
(** [possible_action] is an action associated with a tile*)

type possible_move = vec2
(** [possible_move] is a movement to a tile*)

(** [possible_actions_modifier] is a modifier that changes an attack action. *)
type possible_actions_modifier =
  | ScaleAction of int
  | AddFire of float * int
  | AddDamage of float
  | AugmentToAdjacent

type possible_moves_modifier =
  | ScaleMove of int
      (** [possible_move_function] is a modifier that changes a move action in
          some way. *)

val base_cross_moves : possible_move list
(**[base_cross_moves] is a list containing the most basic movement pattern*)

val range_cross_moves : int -> possible_move list
(** [range_cross_moves r] is a list containing the basic movement patterns
    scaled up by a factor [r]. *)

val base_cross_actions : possible_action list
(**[base_cross_actions] is a list containing the most basic attack pattern,
   which hits all 4 adjacent tiles with fixed damage 1. *)

val range_cross_actions : int -> possible_action list
(** [range_cross_actions r] is a list containing the acting patterns for an
    enemy with fixed damage 1 but variable range [r]. *)

val var_damage_cross_actions : float -> possible_action list
(** [var_damage_cross_actions d] is a list containing the acting patterns for an
    enemy with fixed range 1 (base moves) but variable damage [d] *)

val var_range_damage_cross_actions : int -> float -> possible_action list
(** [var_range_damage_cross_actions r d] is a list containing the actions for an
    enemy with variable range [r] and damage [d] *)

val enemy_attack_type : Enemytype.enemy -> action
(** [enemy_attack_type e] is the effect on the player when an enemy of type [e]
    attacks *)

val enemy_cross_actions : Enemytype.enemy -> possible_action list
(** [enemy_cross_actions] is a list containing the acting patterns for a certain
    enemy type [e] *)

val string_of_modifier : possible_actions_modifier -> string
(** [string_of_modifier m] is the string representation of the attack modifier
    [m]. *)
