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
}

module type ArtIntelligence = sig
  val shoot : player -> (int * int) list * player * result
end

let create_placements p =
  let rec helper (sz : int) (p : player) =
    try
      place_ship p (init_ship sz) (R.int board_size) (R.int board_size)
        (R.bool ())
    with exn -> helper sz p
  in
  let rec loop (i : int) (acc : player) =
    match i with
    | 0 -> acc
    | n when n <= patrol_num -> loop (i - 1) (helper patrol acc |> snd)
    | n when n <= patrol_num + submarine_num ->
        loop (i - 1) (helper submarine acc |> snd)
    | n when n <= patrol_num + submarine_num + destroyer_num ->
        loop (i - 1) (helper destroyer acc |> snd)
    | _ -> loop (i - 1) (helper carrier acc |> snd)
  in
  loop (carrier_num + destroyer_num + submarine_num + patrol_num) p

(** [gen_array p] generates a sorted array in ascending order out of player
    [p]'s board coordinates. *)
let gen_array p =
  fold (fun (x, y) _ acc -> (x, y) :: acc) [] (get_player_board p)
  |> List.rev |> A.of_list

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

let shoot_mid ai p =
  (* If AI has not hit ship recently *)
  if S.is_empty ai.high_priority_stack then (
    let x, y =
      try S.peek ai.low_priority_stack
      with exn -> raise (invalid_arg "Game should be over")
    in
    ai.low_priority_stack <- S.pop ai.low_priority_stack;
    let coords, p, result = fire p x y in

    if result = ShipHit then (
      let next =
        get_adjacents_of_point (x, y)
        |> List.filter (fun (x, y) ->
               match get_cell (get_player_board p) (x, y) with
               | Empty | Ship _ -> true
               | _ -> false)
      in
      ai.high_priority_stack <- S.of_list next;
      ai.low_priority_stack <- S.rem_elements next ai.low_priority_stack);

    (coords, p, result))
  else
    let x, y = S.peek ai.high_priority_stack in
    ai.high_priority_stack <- S.pop ai.high_priority_stack;
    fire p x y

(* ########################################################################## *)
module Make (D : Diff) (P : Player) : ArtIntelligence = struct
  let ai =
    let shuffled_stack = shuffle P.player |> S.of_array in
    { low_priority_stack = shuffled_stack; high_priority_stack = S.empty }

  let rec shoot p =
    match D.difficulty with
    | Easy -> shoot_easy ai p
    | Medium -> shoot_easy ai p
    | Hard -> shoot_easy ai p
end
