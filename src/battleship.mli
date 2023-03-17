type cell
(** Type representing a cell on the board *)

type board
(** Type representing the game board, it is a 10x10 grid *)

type ship
(** Type representing a ship on the board *)

type player
(** Type representing a player. This refers to their board and their ships. *)

exception InvalidPosition

(* type game = { players : player list; mutable current_player : int; } *)
(** Type representing the game itself. *)

val init_board : unit -> board
(** [init_board ()] initializes a new, empty game board. *)

val init_ship : int -> ship
(** [init_ship ()] initializes a new ship *)

val init_player : unit -> player
(** [init_plyaer ()] initializes a player with an empty board *)

(* val init_game : unit -> game *)

val place_ship : player -> ship -> int -> int -> int -> player
(** [place_ship board ship row col dir] is the board after a ship has been
    placed in ([row],[col]) facing direction [dir]. [dir] is 0 if the ship is
    horizontal, 1 if vertical. Requires: [dir] is 0 of 1.*)

val fire : board -> int -> int -> board
(** [fire board row col] is the updated [board] after a shot is fired at
    position ([row],[col]) on the board *)

val get_ships : player -> ship list
(** [get_ships player] is the list of all ships belonging to the [player]*)

val is_game_over : player -> bool
(** [is_game_over player] is whether all of [player]'s ships have been
    destroyed. *)
