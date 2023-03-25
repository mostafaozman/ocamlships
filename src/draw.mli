open Graphics
open Battleship

val write : int -> int -> color -> string -> int -> unit
(** [write x y c s sz] draws the text of [s] with font size [sz] in color [c] at
    pixel position ([x],[y]). *)

val draw_rect : color -> int -> int -> int -> int -> unit
(** [draw_rect c x y w h] draws a rectangle at pixel position ([x],[y]) with
    width [w] and height [h] in color [c]. *)

val draw_cell : color -> int -> int -> unit
(** [draw_cell c x y] draws a cell of color [c] at pixel position ([x],[y]) on
    the grid. *)

val draw_player_board : bool -> player -> unit
(** [draw_player_board p] draws the board associated with player [p]. If the
    board belongs to the current player [self] is true and cells with ships will
    be drawn the same as empty cells. if [self] is false, then ship cells will
    be drawn differently than empty cells. *)

val home : unit -> unit
(** [home ()] draws the start screen of the game. *)

val draw_placing_screen : game ref -> bool -> unit
(** [draw_placing_screen g p] draws the screen of game [g] while player 1 is
    placing if [p] is true, player 2 otherwise. *)
