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
  mutable num_remaining : (int * int) array;
}

module type ArtIntelligence = sig
  val shoot : player -> (int * int) list * player * result
end

(* ############################## Easy AI ################################### *)
let rec create_placements (arr : (int * int) array) (player : player) : player =
  let rec helper (sz : int) (p : player) =
    try
      place_ship p (init_ship sz)
        (R.int board_size, R.int board_size)
        (R.bool ())
    with exn -> helper sz p
  in
  let rec loop (acc : player) ((len, num) : int * int) =
    match num with
    | 0 -> acc
    | _ -> loop (helper len acc |> snd) (len, num - 1)
  in
  A.fold_left (fun acc (len, num) -> loop acc (len, num)) player arr

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

(** [sanitize_for_placing player] is [player] with Hit cells labeled as Empty.
    This is so the simulation knows it can place ships there. *)
let sanitize_for_placing player =
  let b =
    fold
      (fun (x, y) cell acc ->
        match cell with
        | Hit _ -> insert (x, y) Empty acc
        | _ -> insert (x, y) cell acc)
      empty (get_player_board player)
  in
  set_board player b

(** [sanitize player] is [player] with Ship cells labeled as Empty. This is to
    prevent the AI from cheating. *)
let sanitize player =
  let b =
    fold
      (fun (x, y) cell acc ->
        match cell with
        | Ship _ -> insert (x, y) Empty acc
        | _ -> insert (x, y) cell acc)
      empty (get_player_board player)
  in
  set_board player b

(** [is_intersect (x,y) b] is whether the cell at coordinate [(x,y)] on [b] is a
    Hit cell. *)
let is_intersect (x, y) board =
  match get_cell board (x, y) with
  | Empty | Miss | Sunk _ -> false
  | Hit _ -> true
  | _ -> raise (invalid_arg "AI is cheating")

(** [string_of_map fk fv m] is the string representation of [m]. Used for
    debugging only. *)
let string_of_map fk fv m =
  Hashtbl.fold (fun k v acc -> ("(" ^ fk k ^ ":" ^ fv v ^ ")") :: acc) m []
  |> String.concat ","

(** [is_hit_cell c] is whether [c] is a hit cell. *)
let is_hit_cell = function
  | Hit _ -> true
  | _ -> false

(** [sim_helper p pp len dir map] updates [map] with new weights after placing a
    ship of length [len] on every coordinate of [p]'s board facing horizontally
    if [dir] is true, false otherwise. Requires: [pp] is [p] with Hit cells set
    to Empty. *)
let sim_helper player placing_p len dir map =
  for x = 0 to board_size - 1 do
    for y = 0 to board_size - 1 do
      try
        let coords = possible_place placing_p (init_ship len) (x, y) dir in
        if
          List.exists
            (fun c -> c |> get_cell (get_player_board player) |> is_hit_cell)
            coords
        then
          List.iter
            (fun coord ->
              Hashtbl.replace map coord
                (Hashtbl.find map coord * intersect_weight))
            coords
        else
          List.iter
            (fun coord ->
              Hashtbl.replace map coord (Hashtbl.find map coord + 1))
            coords
      with exn -> ()
    done
  done

(** [monte_carlo_sim ai p b] is the Hashtable that gives a weight to every
    coordinate on [b]. *)
let monte_carlo_sim ai player board =
  let map = Hashtbl.create (board_size * board_size) in
  fold (fun (x, y) c acc -> Hashtbl.replace map (x, y) 0) () board;
  let placing_p = sanitize_for_placing player in
  A.iter
    (fun (len, num) ->
      if num <> 0 then sim_helper player placing_p len true map;
      sim_helper player placing_p len false map)
    ai.num_remaining;
  map

(** [random_greater_than a b] randomly determines whether to return a > b or a
    >= b. *)
let random_greater_than a b =
  let c = R.bool () in
  if c then a > b else a >= b

(** [index_in_list x lst] is the index of the first element in [lst] in which
    [fst elem = x]*)
let index_in_list x lst =
  let rec finder x lst acc =
    match lst with
    | [] -> raise (Failure "Not Found")
    | (l, n) :: t -> if l = x then acc else finder x t (acc + 1)
  in
  finder x lst 0

(** [shoot_hard ai p] is the same as [shoot_easy] but utilizes a different
    algorithm to determine where to shoot. *)
let shoot_hard ai p =
  let board = get_player_board p in
  let sanitized_player = sanitize p in
  let new_map = monte_carlo_sim ai sanitized_player board in
  let weight, (x, y) =
    Hashtbl.fold
      (fun (x, y) num_occured (maxer, coord) ->
        match get_cell board (x, y) with
        | Hit _ | Miss | Sunk _ -> (maxer, coord)
        | _ ->
            if random_greater_than num_occured maxer then (num_occured, (x, y))
            else (maxer, coord))
      new_map
      (0, (0, 0))
  in
  print_endline (string_of_int weight);
  let coords, p, result = fire p x y in

  if result = ShipSunk then (
    let len_of_ship =
      match get_cell (get_player_board p) (List.hd coords) with
      | Empty | Hit _ | Miss | Ship _ -> raise (invalid_arg "Not sunk")
      | Sunk { ship } -> !ship.length
    in
    let index = index_in_list len_of_ship (A.to_list ai.num_remaining) in
    ai.num_remaining.(index) <-
      (fst ai.num_remaining.(index), snd ai.num_remaining.(index) - 1);
    print_endline
      (ai.num_remaining |> A.to_list |> List.map string_of_coord
     |> String.concat ";"));

  (coords, p, result)

(* ########################################################################## *)
module Make (D : Diff) (P : Player) : ArtIntelligence = struct
  let ai =
    if D.difficulty <> Hard then
      let shuffled_stack = shuffle P.player |> S.of_array in
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
