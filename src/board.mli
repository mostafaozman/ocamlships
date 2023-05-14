(** Board is the module handling all aspects of a battleship game's board. It
    enables client to create boards and manipulate boards. It also carries some
    helper functions to deal with red-black trees that have (int * int) keys. *)

type ship = { length : int }
(** The type representing a ship. *)

(** The type representing a cell on the board. *)
type cell =
  | Empty
  | Ship of { ship : ship ref }
  | Hit of { ship : ship ref }
  | Sunk of { ship : ship ref }
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

val ( >>@ ) : int * int -> int * int -> bool
(** [ >>@ (x,y) (a,b)] is whether (x,y) is greater than (a,b). Priority is given
    to the second element. *)

val string_of_coord : int * int -> string
(** [string_of_coord x] is the string representation of [x]. *)

val empty : board
(** [empty] is the empty board with no cells. *)

val insert : int * int -> cell -> board -> board
(** [insert k v d] adds the cell [v] to coordinate [k] in [d] and replaces
    anything previously at that coordinate. *)

val find : int * int -> board -> cell option
(** [find k d] is None if [k] is out of bounds of [d] or Some of whatever cell
    is at position [k]. *)

val fold : (int * int -> cell -> 'c -> 'c) -> 'c -> board -> 'c
(** [fold f acc d] applies [f] to all elements in [d] in ascending order
    starting with [acc]. *)

val to_list : board -> ((int * int) * cell) list
(** [to_list d] is [d] as a sorted a association list. *)

val string_of_cell : cell -> string
(** [string_of_cell c] is the string representation of [c]. *)

(** The type representing the color of a node in a rbtree. *)
type color =
  | Red
  | Black

(** The type representing a red-black tree. It satisfies all Red-Black tree
    invariants. *)
type ('k, 'v) rbtree =
  | Node of color * ('k * 'v) * ('k, 'v) rbtree * ('k, 'v) rbtree
  | Leaf

val of_map : (int * int, 'a) Hashtbl.t -> (int * int, 'a) rbtree
(** [of_map m] makes a red-black tree with nodes containing the bindings of [m].
    Useful for debugging.*)

val string_of_tree :
  (int * int -> string) -> ('b -> string) -> (int * int, 'b) rbtree -> string
(** [string_of_tree] is the string representation of a tree. Useful for
    debugging. *)
