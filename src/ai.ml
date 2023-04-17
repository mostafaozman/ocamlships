open Consts
open Battleship
open Board
module R = Random

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
    | n when n < patrol_num -> loop (i - 1) (helper patrol acc)
    | n when n < patrol_num + submarine_num ->
        loop (i - 1) (helper submarine acc)
    | n when n < patrol_num + submarine_num + destroyer_num ->
        loop (i - 1) (helper destroyer acc)
    | _ -> loop (i - 1) (helper carrier acc)
  in
  loop (carrier_num + destroyer_num + submarine_num + patrol_num) p

let rec shoot p =
  try fire p (R.int board_size) (R.int board_size) with exn -> shoot p
