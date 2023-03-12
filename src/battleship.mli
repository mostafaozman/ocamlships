type t
(** The abstract type representing the game board. *)

val board_init : int -> t
(** [board_init i] is the board for the game of size i by i. Requires: [i] <= 12*)
