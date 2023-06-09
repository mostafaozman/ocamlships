(** Battleship is the module responsible for the logic of the battleship game.
    It implements placing, firing, and all interaction a player could have with
    the game. *)

open Board

(** type representing whether a player is an AI or not. *)
type p =
  | AI
  | Player

(** type representing the result of a shot onto a board. *)
type result =
  | ShipHit
  | ShipSunk
  | ShipMissed

type player
(** Type representing a player. Each player has a board and ships. *)

type game
(** Type representing a Battleship game*)

val init_player : p -> player
(** [init_player ()] initializes a player with an empty board *)

val make_game : player -> player -> bool -> game
(** [make_game p1 p2 curr] creates a game with two players, player [p1] will go
    first if [curr] is true, player [p2] will go first otherwise *)

val get_player : game -> bool -> player
(** [get_player g b] is player 1, in game [g], if [b] is true and player 2
    otherwise. *)

val is_player_1 : game -> player -> bool
(** [is_player_1 g p] is whether [p] is player 1 in game [g]. *)

val get_curr_player : game -> player
(** [get_curr_player g] is the current player of game [g]. *)

val string_of_game : game -> string
(** [string_of_game g] is the string representation of game [g]. *)

val empty_player_board : player -> player
(** [empty_player_board p] is player [p] with an empty board. *)

val get_player_board : player -> board
(** [get_player_board p] is the board associated with player [p]*)

val get_cell : board -> int * int -> cell
(** [get_coordinate x] is the cell at board coordinate [x]. Raises
    InvalidPosition if [x] is out of the board's bounds. *)

val get_adjacents_of_point : int * int -> (int * int) list
(** [get_adjacents_of_point (x,y)] is a list of all coordinates adjacent to
    [(x,y)] that are within the board's bounds. *)

val pp : (int * int) list -> string
(** [pp l] is the string representation of a list of coordinates. *)

val num_placed : player -> int -> int
(** [num_placed p i] is the number of ships of length [i] that player [p] has on
    their board. *)

val place_ship :
  player -> ship -> int * int -> bool -> (int * int) list * player
(** [place_ship p ship x y dir] is the tuple with the coordinates of the board
    that have changed AND the updated player after a ship has been placed in
    board position [(x,y)] facing direction [dir]. [dir] is true if the ship is
    horizontal, false if vertical. Raises Invalid Position if position is out of
    bounds, already has ship, or is adjacent to another ship. *)

val possible_place : player -> ship -> int * int -> bool -> (int * int) list
(** [possible_place p ship x y dir] is the coordinates that a ship will occupy
    if it were to be placed on [p]'s board horizontally if [dir] is true,
    vertically otherswise. Raises: InvalidPosition if any position is outside
    the board's bounds or is non-empty. *)

val fire : player -> int -> int -> (int * int) list * player * result
(** [fire player x y] is the tuple with the coordinates of [player]'s board that
    have changed AND the updated player after a shot is fired at board position
    ([x],[y]) AND the result of firing at the coordinate. Raises InvalidPosition
    if position ([x],[y]) is a Hit or Miss cell. *)

val placed_ready : player -> bool
(** [placed_ready player] is whether all of the [player]'s ships have been
    placed on the board. *)

val is_game_over : player -> bool
(** [is_game_over player] is whether all of [player]'s ships have been
    destroyed. *)

val set_board : player -> board -> player
(** [set_board b p] is player [p] with their board set to [b]. *)

val is_ship_cell : cell -> bool
(** [is_ship c] is whether [c] is a ship cell or not. *)

val get_all_ship_coords : player -> (int * int) list
(** [get_all_ship_coords p] is all the coordinates on [p]'s board that have a
    ship on them. *)

val get_same_refs : board -> ship ref -> ((int * int) * cell) list
(** [get_same_refs b s] is all the coordinates and their corresponding cells on
    board [b] that have a ship which shares a memory location with [s]. *)

val set_empty : (int * int) list -> player -> player
(** [set_empty p lst] is [p] with all the coordinates of their board in [lst]
    set to Empty. *)
