(** Board is the module handling all aspects of a battleship game's board. It
    enables client to create board's that respond to player interaction. *)

type ship = { length : int }
(** The type representing a ship. *)

(** The type representing a cell on the board. *)
type cell =
  | Empty
  | Ship of { ship : ship ref }
  | Hit of { ship : ship ref }
  | Sunk of { ship : ship ref }
  | Miss

(** The type representing the color of a node in a rbtree. *)
type color =
  | Red
  | Black

(** The type representing a red-black tree. It satisfies all Red-Black tree
    invariants. *)
type ('k, 'v) rbtree =
  | Node of color * ('k * 'v) * ('k, 'v) rbtree * ('k, 'v) rbtree
  | Leaf

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

val ( >>@ ) : int * int -> int * int -> bool
(** [ >>@ (x,y) (a,b)] is whether (x,y) is greater than (a,b). Priority is given
    to the second element. *)

val string_of_coord : int * int -> string
(** [string_of_coord x] is the string representation of x. *)

val empty : board
(** [empty] is the empty board with no cells. *)

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

val of_map : ('a * 'b, 'c) Hashtbl.t -> ('a * 'b, 'c) rbtree

val string_of_tree :
  ('a -> string) -> ('b -> string) -> ('a, 'b) rbtree -> string
