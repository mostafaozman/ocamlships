open Battleship

val create_placements : player -> player
(** [create_placements p] is [p] with their board occupied by a set number of
    ships. Requires: [p]'s board is empty. *)

(** Type representing how good at Battleship the AI will be. *)
type difficulty =
  | Easy
  | Medium
  | Hard

(** Wrapper module type for difficulty. *)
module type Diff = sig
  val difficulty : difficulty
end

val gen_diff_module : difficulty -> (module Diff)
(** [gen_diff_module d] is a first-class Diff module with difficulty set to [d]. *)

(** Wrapper module for a player. *)
module type Player = sig
  val player : player
end

val gen_player_module : player -> (module Player)
(** [gen_player_module p] is a first-class Player module with player set to [p]. *)

type ai = {
  mutable arr : (int * int) array;
  mutable turn : int;
}
(** ai is the type representing an AI. *)

(** type representing an AI and its possible actions. *)
module type ArtIntelligence = sig
  val ai : ai
  val shoot : player -> (int * int) list * player
end

module Make (_ : Diff) (_ : Player) : ArtIntelligence
