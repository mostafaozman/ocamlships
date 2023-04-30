open Consts
open Battleship
open Board
module A = Array
module S = Stack
module R = Random

let _ = R.self_init ()

type difficulty =
  | Easy
  | Medium
  | Hard

module type Diff = sig
  val difficulty : difficulty
end

let gen_diff_module diff =
  (module struct
    let difficulty = diff
  end : Diff)

module type Player = sig
  val player : player
end

let gen_player_module player =
  (module struct
    let player = player
  end : Player)

type ai = {
  mutable low_priority_stack : (int * int) S.t;
  mutable high_priority_stack : (int * int) S.t;
  mutable num_remaining : int array;
}

module type ArtIntelligence = sig
  val shoot : player -> (int * int) list * player * result
end

(* ############################## Easy AI ################################### *)
let rec create_placements arr player =
  let rec helper (sz : int) (p : player) =
    try
      place_ship p (init_ship sz)
        (R.int board_size, R.int board_size)
        (R.bool ())
    with exn -> helper sz p
  in
  let rec loop (i : int) (acc : player) =
    match i with
    | 0 -> acc
    | n when n <= arr.(3) -> loop (i - 1) (helper patrol acc |> snd)
    | n when n <= arr.(2) + arr.(3) -> loop (i - 1) (helper submarine acc |> snd)
    | n when n <= arr.(1) + arr.(2) + arr.(3) ->
        loop (i - 1) (helper destroyer acc |> snd)
    | _ -> loop (i - 1) (helper carrier acc |> snd)
  in
  loop (arr.(0) + arr.(1) + arr.(2) + arr.(3)) player

(** [gen_array p] generates a sorted array in ascending order out of player
    [p]'s board coordinates. *)
let gen_array p =
  get_player_board p |> to_list
  |> List.map (fun (coord, cell) -> coord)
  |> A.of_list

(** [swap a i j] swaps the values of index [i] and [j] in array [a]. *)
let swap a i j =
  let temp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- temp

(** [shuffle p] generates a shuffled array of coordinates out of player [p]'s
    board. *)
let shuffle p =
  let shuffle_helper (arr : (int * int) array) =
    for i = A.length arr - 1 downto 1 do
      let j = R.int (i + 1) in
      swap arr i j
    done
  in
  let arr = gen_array p in
  shuffle_helper arr;
  arr

(** [shoot_easy ai p] is a tuple containing a list of coordinates of the cells
    of player [p]'s board that have changed AND an updated player after [p]'s
    board has been fired at AND the result of the shot. Utilizes a naively
    random algorithm to determine where to shoot. *)
let shoot_easy ai p =
  let stack = ai.low_priority_stack in
  let x, y = S.peek stack in
  ai.low_priority_stack <- S.pop stack;
  fire p x y

(* ################################## Mid AI ################################ *)

(** [shoot_mid_helper (x,y) p] is the list of coordinates that are adjacent to
    [(x,y)] but have not been shot at on [p]'s board. *)
let shoot_mid_helper (x, y) p =
  get_adjacents_of_point (x, y)
  |> List.filter (fun (x, y) ->
         match get_cell (get_player_board p) (x, y) with
         | Empty | Ship _ -> true
         | _ -> false)

(** [shoot_mid ai p] is the same as shoot_easy but utilizes an improved
    algorithm for determining where to shoot. *)
let shoot_mid ai p =
  if S.is_empty ai.high_priority_stack then (
    let x, y =
      try S.peek ai.low_priority_stack
      with exn -> raise (invalid_arg "Game should be over")
    in
    ai.low_priority_stack <- S.pop ai.low_priority_stack;
    let coords, p, result = fire p x y in

    if result = ShipHit then (
      let next = shoot_mid_helper (x, y) p in
      ai.high_priority_stack <- S.of_list next;
      ai.low_priority_stack <- S.rem_elements next ai.low_priority_stack);

    (coords, p, result))
  else
    let x, y = S.peek ai.high_priority_stack in
    ai.high_priority_stack <- S.pop ai.high_priority_stack;
    let coords, p, result = fire p x y in
    if result = ShipHit then (
      let next = shoot_mid_helper (x, y) p in
      ai.high_priority_stack <-
        S.append (S.of_list next) (S.rem_elements next ai.high_priority_stack);
      ai.low_priority_stack <- S.rem_elements next ai.low_priority_stack);
    (coords, p, result)

(* ################################ Hard AI ################################# *)

(** [new_ship_spots ai player] is all the coordinates on [player]'s board that
    have a ship on them after randomly generating possible configurations for
    it. *)
let new_ship_spots ai player =
  let new_player = create_placements ai.num_remaining player in
  fold
    (fun (x, y) cell acc ->
      match cell with
      | Ship _ -> (x, y) :: acc
      | _ -> acc)
    []
    (get_player_board new_player)

let sanitize player =
  let b =
    fold
      (fun (x, y) cell acc ->
        match cell with
        | Ship _ | Hit _ -> insert (x, y) Empty acc
        | _ -> insert (x, y) cell acc)
      empty (get_player_board player)
  in
  set_board player b

let monte_carlo_sim ai player samples =
  let rec sim_helper ai player samples acc =
    match samples with
    | 0 -> acc
    | _ ->
        sim_helper ai player (samples - 1)
          (new_ship_spots ai (sanitize player) :: acc)
  in
  List.flatten (sim_helper ai player samples [])

let shoot_hard ai p =
  let board = get_player_board p in
  let map = Hashtbl.create (board_size * board_size) in
  fold (fun (x, y) c acc -> Hashtbl.replace map (x, y) 0) () board;
  List.fold_left
    (fun () (x, y) -> Hashtbl.replace map (x, y) (Hashtbl.find map (x, y) + 1))
    () (monte_carlo_sim ai p 500);
  let weight, (x, y) =
    Hashtbl.fold
      (fun (x, y) num_occured (maxer, coord) ->
        match get_cell board (x, y) with
        | Hit _ | Miss | Sunk _ -> (maxer, coord)
        | _ ->
            if num_occured >= maxer then (num_occured, (x, y))
            else (maxer, coord))
      map
      (0, (0, 0))
  in
  let coords, p, result = fire p x y in

  print_endline (pp coords);
  (if result = ShipSunk then
   let len_of_ship =
     match get_cell (get_player_board p) (List.hd coords) with
     | Empty | Hit _ | Miss | Ship _ -> raise (invalid_arg "Not sunk")
     | Sunk { ship } -> !ship.length
   in
   let index = -len_of_ship + carrier in
   ai.num_remaining.(index) <- ai.num_remaining.(index) - 1);

  (coords, p, result)

(* ########################################################################## *)
module Make (D : Diff) (P : Player) : ArtIntelligence = struct
  let ai =
    let shuffled_stack = shuffle P.player |> S.of_array in
    if D.difficulty <> Hard then
      {
        low_priority_stack = shuffled_stack;
        high_priority_stack = S.empty;
        num_remaining = [||];
      }
    else
      {
        low_priority_stack = S.empty;
        high_priority_stack = S.empty;
        num_remaining = ship_num_arr;
      }

  let rec shoot p =
    match D.difficulty with
    | Easy -> shoot_easy ai p
    | Medium -> shoot_mid ai p
    | Hard -> shoot_hard ai p
end
