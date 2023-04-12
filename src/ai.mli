open Battleship

val create_placements : player -> player
(** [create_placements p] is [p] with their board occupied by a set number of
    ships. Requires: [p]'s board is empty. *)

val ai_fire : player -> player
(** [ai_fire p] fires at a position on p's board. *)
