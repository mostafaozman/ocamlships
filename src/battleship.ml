(* Implement functions from battleship.mli *)
let cBOARD_SIZE = 10
let cCARRIER = 5
let cDESTROYER = 4
let cSUBMARINE = 3
let cPATROL = 2
let cSHIP_SIZES = [ cCARRIER; cDESTROYER; cSUBMARINE; cPATROL ]

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
    let dx = abs (snd a - fst a) in
    let dy = abs (snd x - fst x) in
    dx <= 1 && dy <= 1

let extract_pos (cell : cell) =
  match cell with
  | Empty t | Hit t | Miss t -> t
  | Ship t -> t.position

(** [pos_of_ship board ship row col dir] is all the coordinates on board which
    [ship] will occupy when placed on position ([row],[col]) facing direction
    [dir]. Raises InvalidPosition if any coordinates are out of bounds *)
let pos_of_ship ship row col dir =
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
      if dir = 0 then List.init 5 (fun x -> (row + (-2 + x), col))
      else List.init 5 (fun y -> (row, col + (-2 + y)))
  | 3 | 4 ->
      if dir = 0 then List.init ship.length (fun x -> (row + (-1 + x), col))
      else List.init ship.length (fun x -> (row, col + (-1 + x)))
  | 2 ->
      if dir = 0 then [ (row, col); (row + 1, col) ]
      else [ (row, col); (row, col + 1) ]
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

let place_ship player ship row col dir =
  let ship_spots = pos_of_ship ship row col dir in
  {
    player with
    board =
      (if check_position player.board ship_spots then
       List.map
         (fun y ->
           List.map
             (fun x ->
               let pos_x = extract_pos x in
               if List.mem pos_x ship_spots then
                 Ship { position = (fst pos_x, snd pos_x); ship = ref ship }
               else x)
             y)
         player.board
      else player.board);
  }

let fire board row col = raise (Failure "init_board Unimplemented")
let get_ships player = player.ships
let is_game_over player = raise (Failure "init_board Unimplemented")
