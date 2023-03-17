(* Implement functions from battleship.mli *)
open GMain
open Consts

exception InvalidPosition

type ship = {
  length : int;
  mutable hits : int;
}

type cell =
  | Empty
  | Ship of ship ref
  | Hit
  | Miss

type board = cell list list

type player = {
  board : board;
  ships : ship list;
}

type game = {
  players : player list;
  mutable current_player : int;
}

let init_board () =
  List.init cBOARD_SIZE (fun _ -> List.init cBOARD_SIZE (fun _ -> Empty))

let init_ship len = { length = len; hits = 0 }

let create_ship x =
  match x with
  | 0 -> init_ship cCARRIER
  | 1 | 2 -> init_ship cDESTROYER
  | 3 | 4 | 5 -> init_ship cSUBMARINE
  | 6 | 7 | 8 | 9 -> init_ship cPATROL
  | _ -> raise (invalid_arg "Too many ships")

let init_player () = { board = init_board (); ships = List.init 10 create_ship }
let place_ship player ship row col dir = { player with board = init_board () }
let fire board row col = raise (Failure "init_board Unimplemented")
let get_ships player = player.ships
let is_game_over player = raise (Failure "init_board Unimplemented")
