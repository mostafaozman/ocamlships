open Consts
open Battleship
open Board
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

let easy = gen_module Easy
let medium = gen_module Medium
let hard = gen_module Hard

let d =
  let module D = (val easy) in
  D.difficulty

let set_difficulty diff =
  let diff_ai = gen_module diff in
  let module D = (val diff_ai) in
  D.difficulty

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
