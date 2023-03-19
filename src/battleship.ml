(* Implement functions from battleship.mli *)
open Consts

exception InvalidPosition

type ship = {
  length : int;
  mutable hits : int;
}

type cell =
  | Empty of (int * int)
  | Ship of {
      position : int * int;
      ship : ship ref;
    }
  | Hit of (int * int)
  | Miss of (int * int)

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
  List.init cBOARD_SIZE (fun y -> List.init cBOARD_SIZE (fun x -> Empty (x, y)))

let init_ship len = { length = len; hits = 0 }

let create_ship x =
  match x with
  | 0 -> init_ship cCARRIER
  | 1 | 2 -> init_ship cDESTROYER
  | 3 | 4 | 5 -> init_ship cSUBMARINE
  | 6 | 7 | 8 | 9 -> init_ship cPATROL
  | _ -> raise (invalid_arg "Too many ships")

let get_ship_length s = s.length
let init_player () = { board = init_board (); ships = List.init 10 create_ship }

(** [is_adjacent lst x a] is whether the element at position [x] in the matrix
    is adjacent, including diagonals, to the element at position [a].*)
let rec is_adjacent (lst : cell list list) (x : int * int) (a : int * int) :
    bool =
  if
    fst x < 0
    || fst x >= cBOARD_SIZE
    || snd x < 0
    || snd x >= cBOARD_SIZE
    || fst a < 0
    || fst a >= cBOARD_SIZE
    || snd a < 0
    || snd a >= cBOARD_SIZE
  then raise InvalidPosition
  else
    let dx = abs (fst x - fst a) in
    let dy = abs (snd x - snd a) in
    dx <= 1 && dy <= 1

let extract_pos (cell : cell) =
  match cell with
  | Empty t | Hit t | Miss t -> t
  | Ship t -> t.position

(** [pos_of_ship board ship x y dir] is all the coordinates on board which
    [ship] will occupy when placed on position ([x],[y]) facing direction [dir].
    Raises InvalidPosition if any coordinates are out of bounds *)
let pos_of_ship ship x y dir =
  let rec check_bounds acc lst =
    match lst with
    | [] -> acc
    | h :: t ->
        if fst h < 0 || fst h > cBOARD_SIZE || snd h < 0 || snd h > cBOARD_SIZE
        then raise InvalidPosition
        else check_bounds (h :: acc) t
  in
  (match ship.length with
  | 5 ->
      if dir = 0 then List.init 5 (fun i -> (x + (-2 + i), y))
      else List.init 5 (fun y -> (x, y + (-2 + y)))
  | 3 | 4 ->
      if dir = 0 then List.init ship.length (fun i -> (x + (-1 + i), y))
      else List.init ship.length (fun i -> (x, y + (-1 + i)))
  | 2 -> if dir = 0 then [ (x, y); (x + 1, y) ] else [ (x, y); (x, y + 1) ]
  | _ -> [])
  |> check_bounds []

let rec check_position (board : board) (ship_spots : (int * int) list) =
  match board with
  | [] -> true
  | h :: t ->
      if
        List.for_all
          (fun x ->
            let pos_x = extract_pos x in
            if List.mem pos_x ship_spots then x = Empty (fst pos_x, snd pos_x)
            else true)
          h
      then check_position t ship_spots
      else false

let place_ship player ship x y dir =
  let updated_board board ship ship_spots =
    if check_position board ship_spots then
      List.map
        (fun y ->
          List.map
            (fun x ->
              let pos_x = extract_pos x in
              if List.mem pos_x ship_spots then
                Ship { position = (fst pos_x, snd pos_x); ship }
              else x)
            y)
        board
    else board
  in
  let ship_spots = pos_of_ship ship x y dir in
  { player with board = updated_board player.board (ref ship) ship_spots }

let fire board x y = raise (Failure "init_board Unimplemented")
let get_ships player = player.ships
let is_game_over player = raise (Failure "init_board Unimplemented")
