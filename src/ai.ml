open Consts
open Battleship
open Board
module A = Array
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
  mutable arr : (int * int) array;
  mutable turn : int;
}

module type ArtIntelligence = sig
  val shoot : player -> (int * int) list * player
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
    board has been fired at. Utilizes a naively random algorithm to determine
    where to shoot. *)
let shoot_easy ai p =
  let x, y = ai.arr.(ai.turn) in
  ai.turn <- ai.turn + 1;
  fire p x y

(* ########################################################################## *)
module Make (D : Diff) (P : Player) : ArtIntelligence = struct
  let ai = { arr = shuffle P.player; turn = 0 }

  let rec shoot p =
    match D.difficulty with
    | Easy -> shoot_easy ai p
    | Medium -> shoot_easy ai p
    | Hard -> shoot_easy ai p
end
