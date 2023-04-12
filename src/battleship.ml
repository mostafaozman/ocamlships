(* Implement functions from battleship.mli *)
open Consts

exception InvalidPosition of string

type ship = {
  length : int;
  adjacents : (int * int) list;
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
}

type game = {
  players : player * player;
  mutable current_player : bool;
}

let init_board () =
  List.init board_size (fun y -> List.init board_size (fun x -> Empty (x, y)))

let init_ship len = { length = len; adjacents = [] }
let get_ship_length s = s.length
let init_player name = { name; board = init_board () }
let get_player g b = if b then fst g.players else snd g.players
let get_player_board p = p.board
let make_game p1 p2 curr = { players = (p1, p2); current_player = curr }

let string_of_coord x =
  "(" ^ string_of_int (fst x) ^ "," ^ string_of_int (snd x) ^ ")"

(** [pp l] is the string representation of a list of coordinates. *)
let pp l = "[" ^ String.concat ";" (List.map string_of_coord l) ^ "]"

(** [is_adjacent lst x a] is whether the element at position [x] in the matrix
    is adjacent, including diagonals, to the element at position [a]. *)
let rec is_adjacent (x : int * int) (a : int * int) : bool =
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

let get_coordinate b x = List.find (fun a -> extract_pos a = x) (List.flatten b)

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
  begin
    (match ship.length with
    | 5 ->
        if dir = 0 then List.init 5 (fun i -> (x + (-2 + i), y))
        else List.init 5 (fun y -> (x, y + (-2 + y)))
    | 4 | 3 ->
        if dir = 0 then List.init ship.length (fun i -> (x + (-1 + i), y))
        else List.init ship.length (fun i -> (x, y + (-1 + i)))
    | 2 -> if dir = 0 then [ (x, y); (x + 1, y) ] else [ (x, y); (x, y + 1) ]
    | _ -> [])
    |> check_bounds []
  end

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

(** [get_adjacents s] is all coordinates that are adjacent to the coordinates in
    [s] within the board's bounds. *)
let get_adjacents spots =
  let get_adjacents_of_point (x, y) =
    let top = (x, y + 1) in
    let left = (x - 1, y) in
    let bottom = (x, y - 1) in
    let right = (x + 1, y) in
    let top_left = (x - 1, y + 1) in
    let bottom_left = (x - 1, y - 1) in
    let top_right = (x + 1, y + 1) in
    let bottom_right = (x + 1, y - 1) in
    [ top; left; bottom; right; top_left; bottom_left; top_right; bottom_right ]
  in
  List.map get_adjacents_of_point spots
  |> List.flatten |> List.sort_uniq compare
  |> List.filter (fun (x, y) ->
         (not (List.mem (x, y) spots))
         && x >= 0 && x < board_size && y >= 0 && y < board_size)

(** [pred b c] is true if the coordinate c is a non-ship cell in board [b].
    Raises: InvalidPosition otherwise.*)
let pred board c =
  match get_coordinate board c with
  | Empty _ | Hit _ | Miss _ -> true
  | Ship _ ->
      raise (InvalidPosition (string_of_coord c ^ " is an adjacent ship"))

(** [placer b s sp] is the board after the ship [s] which occupies the positions
    in [sp] is placed on board [b]. *)
let placer board ship ship_spots =
  let ship = ref ship in
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
    let adjacency_list = get_adjacents ship_spots in
    if
      is_valid_position board ship_spots
      && List.for_all (pred board) adjacency_list
    then placer board { ship with adjacents = adjacency_list } ship_spots
    else board
  in
  let ship_spots = pos_of_ship ship x y dir in
  { player with board = updated_board player.board ship ship_spots }

let num_placed player i =
  let get_ship_cell i cell =
    match cell with
    | Empty _ | Hit _ | Miss _ -> false
    | Ship t -> if !(t.ship).length = i then true else false
  in
  let get_unique_ship_refs acc x =
    match x with
    | Empty _ | Hit _ | Miss _ -> acc
    | Ship t -> if List.memq t.ship acc then acc else t.ship :: acc
  in
  List.flatten player.board
  |> List.filter (get_ship_cell i)
  |> List.fold_left get_unique_ship_refs []
  |> List.length

(** [get_same_refs b s] is all the cells on board [b] that share a memory
    location with [s]. *)
let get_same_refs board ship =
  List.flatten board
  |> List.filter (fun cell ->
         match cell with
         | Empty _ | Hit _ | Miss _ -> false
         | Ship { position = pos; ship = s } -> s == ship)

(** [fire_transform b x y t] is the board after the cell at position ([x],[y])
    on the board [b] is transformed into [t]*)
let fire_transform board x y transformation =
  List.init board_size (fun h ->
      List.init board_size (fun w ->
          if (w, h) = (x, y) then transformation else get_coordinate board (w, h)))

(** [adjacent_transform b s coord] is the board [b] after all cells adjacent to
    [s] become a Miss and the cell at [coord] becomes a Hit. *)
let adjacent_transform board ship (x, y) =
  List.init board_size (fun h ->
      List.init board_size (fun w ->
          if List.mem (w, h) ship.adjacents then Miss (w, h)
          else if (w, h) = (x, y) then Hit (w, h)
          else get_coordinate board (w, h)))

let fire p x y =
  let fire_helper board x y =
    match get_coordinate board (x, y) with
    | Empty _ -> fire_transform board x y (Miss (x, y))
    | Ship { position; ship } ->
        let same_refs = List.length (get_same_refs board ship) in
        if same_refs = 1 then (
          print_endline (string_of_int same_refs);
          adjacent_transform board !ship (x, y))
        else (
          print_endline "Strange";
          fire_transform board x y (Hit (x, y)))
    | _ -> raise (InvalidPosition (string_of_coord (x, y)))
  in
  { p with board = fire_helper p.board x y }

let is_game_over player =
  raise (Failure "Battleship.is_game_over Unimplemented")
