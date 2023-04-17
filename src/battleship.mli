open Board

type player
(** Type representing a player. Each player has a board and ships. *)

type game
(** Type representing a Battleship game*)

val init_player : string -> player
(** [init_player ()] initializes a player with an empty board *)

val make_game : player -> player -> bool -> game
(** [make_game p1 p2 curr] creates a game with two players, player [p1] will go
    first if [curr] is true, player [p2] will go first otherwise *)

val get_player : game -> bool -> player
(** [get_player g b] is player 1, in game [g], if [b] is true and player 2
    otherwise. *)

val get_player_board : player -> board
(** [get_player_board p] is the board associated with player [p]*)

val num_placed : player -> int -> int
(** [num_placed p i] is the number of ships of length [i] that player [p] has on
    their board. *)

val place_ship :
  player -> ship -> int -> int -> bool -> (int * int) list * player
(** [place_ship board ship x y dir] is the board after a ship has been placed in
    board position ([x],[y]) facing direction [dir]. [dir] is true if the ship
    is horizontal, false if vertical. Raises Invalid Position if position is out
    of bounds, already has ship, or is adjacent to another ship. *)

val fire : player -> int -> int -> player
(** [fire board x y] is the updated [board] after a shot is fired at board
    position ([x],[y]) on the board. Raises InvalidPosition if position
    ([x],[y]) is a Hit or Miss cell. *)

val placed_ready : player -> bool
(**[placed_ready player] is whether all of the [player]'s ships have been placed
   on the board*)

val is_game_over : player -> bool
(** [is_game_over player] is whether all of [player]'s ships have been
    destroyed. *)
