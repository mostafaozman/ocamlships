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

let gen_module diff =
  (module struct
    let difficulty = diff
  end : Diff)

let set_difficulty diff = gen_module diff

(** [gen_array p] generates a sorted array in ascending order out of player
    [p]'s board. *)
let gen_array p =
  fold (fun (x, y) _ acc -> (x, y) :: acc) [] (get_player_board p)
  |> List.rev |> A.of_list

(** [shuffle p] generates a shuffled array out of player [p]'s board. *)
let shuffle p =
  let shuffle_helper (arr : (int * int) array) =
    for i = A.length arr - 1 downto 1 do
      let j = R.int (i + 1) in
      let temp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- temp
    done
  in
  let arr = gen_array p in
  shuffle_helper arr;
  arr

module AI (D : Diff) = struct
  let rec shoot p =
    match D.difficulty with
    | Easy -> (
        try fire p (R.int board_size) (R.int board_size) with exn -> shoot p)
    | Medium -> (
        try fire p (R.int board_size) (R.int board_size) with exn -> shoot p)
    | Hard -> (
        try fire p (R.int board_size) (R.int board_size) with exn -> shoot p)
end

let rec shoot p =
  try fire p (R.int board_size) (R.int board_size) with exn -> shoot p

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
