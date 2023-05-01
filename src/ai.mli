open Battleship

type ai
(** ai is the type representing an AI. *)

(** Type representing how good at Battleship the AI will be. *)
type difficulty =
  | Easy
  | Medium
  | Hard

(** Wrapper module type for difficulty. *)
module type Diff = sig
  val difficulty : difficulty
end

(** Wrapper module for a player. *)
module type Player = sig
  val player : player
end

(** type representing an AI firing at a player's board. *)
module type ArtIntelligence = sig
  val shoot : player -> (int * int) list * player * result
end

val gen_diff_module : difficulty -> (module Diff)
(** [gen_diff_module d] is a first-class Diff module with difficulty set to [d]. *)

val gen_player_module : player -> (module Player)
(** [gen_player_module p] is a first-class Player module with player set to [p]. *)

val create_placements : int array -> player -> player
(** [create_placements arr p] is [p] with their board occupied by the number of
    ships specified in. Requires: [arr] is of length 4 and index 0 is the number
    of length 5 ships, index 1 is length 4, and so on. *)

(** [Make] is the functor that determines how the AI plays. *)
module Make : functor (D : Diff) (P : Player) -> ArtIntelligence
