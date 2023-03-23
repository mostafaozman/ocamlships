(* Implement functions from battleship.mli *)
open Consts

exception InvalidPosition of string

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
  name : string;
  board : board;
  ships : ship list;
}

type game = {
  players : player * player;
  mutable current_player : int;
}

let init_board () =
  List.init board_size (fun y -> List.init board_size (fun x -> Empty (x, y)))

let init_ship len = { length = len; hits = 0 }
let get_ship_length s = s.length

let init_player name =
  let create_ship x =
    match x with
    | 0 -> init_ship carrier
    | 1 | 2 -> init_ship destroyer
    | 3 | 4 | 5 -> init_ship submarine
    | 6 | 7 | 8 | 9 -> init_ship patrol
    | _ -> raise (invalid_arg "Too many ships")
  in
  { name; board = init_board (); ships = List.init 10 create_ship }

let get_player g i = if i = 1 then fst g.players else snd g.players
let get_player_board p = p.board

let init_game name1 name2 =
  { players = (init_player name1, init_player name2); current_player = 0 }

let string_of_coord x =
  "(" ^ string_of_int (fst x) ^ "," ^ string_of_int (snd x) ^ ")"

(** [is_adjacent lst x a] is whether the element at position [x] in the matrix
    is adjacent, including diagonals, to the element at position [a]. *)
let rec is_adjacent (lst : cell list list) (x : int * int) (a : int * int) :
    bool =
  if fst x < 0 || fst x >= board_size || snd x < 0 || snd x >= board_size then
    raise (InvalidPosition (string_of_coord x))
  else if fst a < 0 || fst a >= board_size || snd a < 0 || snd a >= board_size
  then raise (InvalidPosition (string_of_coord a))
  else
    let dx = abs (fst x - fst a) in
    let dy = abs (snd x - snd a) in
    dx <= 1 && dy <= 1

(** [extract_pos c] is the coordinates associated to [c]*)
let extract_pos (cell : cell) =
  match cell with
  | Empty t | Hit t | Miss t -> t
  | Ship t -> t.position

let get_coordinate b x =
  (List.find (fun a -> extract_pos a = x)) (List.flatten b)

(** [pos_of_ship board ship x y dir] is all the coordinates which [ship] will
    occupy when placed on grid position ([x],[y]) facing direction [dir]. Raises
    InvalidPosition if any coordinates are out of bounds *)
let pos_of_ship ship x y dir =
  let rec check_bounds acc lst =
    match lst with
    | [] -> acc
    | h :: t ->
        if fst h < 0 || fst h >= board_size || snd h < 0 || snd h >= board_size
        then raise (InvalidPosition (string_of_coord h))
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

(** [is_valid_position b s] is whether all in [s] are empty.*)
let rec is_valid_position (board : board) (ship_spots : (int * int) list) =
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
      then is_valid_position t ship_spots
      else false

let place_ship_helper board ship ship_spots =
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

let place_ship player ship x y dir =
  let updated_board board ship ship_spots =
    if is_valid_position board ship_spots then
      place_ship_helper board ship ship_spots
    else board
  in
  let ship_spots = pos_of_ship ship x y dir in
  { player with board = updated_board player.board (ref ship) ship_spots }

let fire board x y = raise (Failure "init_board Unimplemented")
let get_ships player = player.ships
let is_game_over player = raise (Failure "init_board Unimplemented")
