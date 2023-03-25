type ship
(** Type representing a ship on the board *)

(** Type representing a cell on the board *)
type cell =
  | Empty of (int * int)
  | Ship of {
      position : int * int;
      ship : ship ref;
    }
  | Hit of (int * int)
  | Miss of (int * int)

type board = cell list list
(** Type representing the game board, it is a 14x14 grid *)

type player
(** Type representing a player. Each player has a board and ships. *)

type game
(** Type representing a Battleship game*)

exception InvalidPosition of string

(* type game = { players : player list; mutable current_player : int; } *)
(** Type representing the game itself. *)

val init_board : unit -> board
(** [init_board ()] initializes a new, empty game board. *)

val string_of_coord : int * int -> string
(** [string_of_coord x] is the string representation of x. *)

val init_ship : int -> ship
(** [init_ship i] initializes a new ship of length [i] *)

val get_ship_length : ship -> int
(** [get_ship_length s] is the length of the ship [s] *)

val init_player : string -> player
(** [init_player ()] initializes a player with an empty board *)

val make_game : player -> player -> bool -> game
(** [init_game p1 p2 curr] creates a game with two players, player [p1] will go
    first if [curr] is true, player [p2] will go first otherwise *)

val get_player : game -> bool -> player
(** [get_player g b] is player 1, in game [g], if [b] is true and player 2
    otherwise. *)

val get_player_board : player -> board
(** [get_player_board p] is the board associated with player [p]*)

val num_placed : player -> int -> int
(** [num_placed p i] is the number of ships of length [i] that player [p] has on
    their board. *)

val get_coordinate : board -> int * int -> cell
(** [get_coordinate x] is the cell at board coordinate [x]. *)

val place_ship : player -> ship -> int -> int -> int -> player
(** [place_ship board ship x y dir] is the board after a ship has been placed in
    board position ([x],[y]) facing direction [dir]. [dir] is 0 if the ship is
    horizontal, 1 if vertical. Requires: [dir] is 0 or 1.*)

val fire : board -> int -> int -> board
(** [fire board x y] is the updated [board] after a shot is fired at board
    position ([x],[y]) on the board *)

val get_ships : player -> ship list
(** [get_ships player] is the list of all ships belonging to the [player]*)

val is_game_over : player -> bool
(** [is_game_over player] is whether all of [player]'s ships have been
    destroyed. *)
