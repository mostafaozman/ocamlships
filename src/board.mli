type ship = {
  length : int;
  adjacents : (int * int) list;
}
(** The type representing a ship. *)

(** The type representing a cell on the board. *)
type cell =
  | Empty
  | Ship of { ship : ship ref }
  | Hit
  | Miss

type board
(** The type representing the board of a game of Battleship. *)

exception InvalidPosition of string

val init_board : unit -> board
(** [init_board ()] initializes a new, empty game board. *)

val init_ship : int -> ship
(** [init_ship l] initializes a new ship of length [l]. *)

val ( @<< ) : int * int -> int * int -> bool
(** [ @<< (x,y) (a,b)] is whether (x,y) is less than (a,b). Priority is given to
    the second element. *)

val ( @<< ) : int * int -> int * int -> bool
(** [ >>@ (x,y) (a,b)] is whether (x,y) is greater than (a,b). Priority is given
    to the second element. *)

val string_of_coord : int * int -> string
(** [string_of_coord x] is the string representation of x. *)

val insert : int * int -> cell -> board -> board
(** [insert k v d] adds the binding (k,v) to [d] and replaces any previous
    binding. *)

val find : int * int -> board -> cell option
(** [find k d] is None if [k] is out of bounds of [d] or Some of whatever cell
    is at position [k]. *)

val fold : (int * int -> cell -> 'a -> 'a) -> 'a -> board -> 'a
(** [fold f acc d] applies [f] to all elements in [d] in ascending order
    starting with [acc]. *)

val to_list : board -> ((int * int) * cell) list
(** [to_list d] is [d] as a sorted a association list. *)
