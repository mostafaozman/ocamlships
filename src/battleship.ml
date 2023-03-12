(* Implement functions from battleship.mli *)
open Graphics

type cell = 
  (** type cell represents a cell within the board. *)
{
  point : int * int;
  width : int;
  height : int;
  color : Graphics.color;
}

type t = { cells : cell list list }

let rec board_init (i : int) = raise (Failure "Unimplemented")
