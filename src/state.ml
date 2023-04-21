(* Implement functions from state.mli *)
open Graphics
open Consts
open Board
open Battleship
open Ai
open Draw

type state =
  | START
  | PLACING
  | PLAY

let state = ref START
let player_board = init_player "Player"
let op_board = init_player "AI" |> create_placements
let game = ref (make_game player_board op_board true)

(** [convert x y] is the grid coordinate associated with pixel position
    ([x],[y]). None if coordinate is outside the grid. *)
let convert x y =
  if
    x < background_llx
    || x > background_length + background_llx - box_off
    || y < background_lly || y > background_tly
  then None
  else
    let r = (background_tly - y - box_off) / box_size in
    let c = (x - background_llx - box_off) / box_size in
    Some (c, r)

(** [quit ()] exits the program with exit status 0. *)
let quit () = exit 0

(** [go_start g] changes state to PLACING and draws the board of player 1 in
    [g]. *)
let go_start game =
  state := PLACING;
  clear_graph ();
  draw_player_board true (get_player game true)

let go_play game =
  state := PLAY;
  clear_graph ()

(** [start_loop g] is the start screen of game [g]. *)
let start_loop game =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  if st.key == 'q' then quit ();
  (* If condition for start box *)
  if
    (st.mouse_x >= 200 && st.mouse_x <= 600)
    && st.mouse_y >= 300 && st.mouse_y <= 425
  then (
    go_start !game;
    draw_placing_screen game true)
  else if
    (* If condition for quit box *)
    (st.mouse_x >= 200 && st.mouse_x <= 600)
    && st.mouse_y >= 100 && st.mouse_y <= 225
  then quit ()

(** [place_loop g p i] draws the board of player 1 if [p] is true, player 2
    otherwise, after they have placed a ship of length [i] in game [g]. *)
let rec place_loop game p i dir =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  clear_graph ();
  draw_placing_screen game p;
  let tup = convert st.mouse_x st.mouse_y in
  match tup with
  | None ->
      write 200 760 black "Select position on board" 25;
      place_loop game p i dir
  | Some tup -> (
      try
        let ship_coords, updated_player =
          place_ship (get_player !game p) (init_ship i) (fst tup) (snd tup) dir
        in
        if p then (
          game := make_game updated_player (get_player !game (not p)) p;
          update_cells true ship_coords)
        else game := make_game (get_player !game (not p)) updated_player p;
        update_cells true ship_coords
      with e ->
        write 200 760 black "Can't place ship there" 30;
        place_loop game p i dir)

(** [button_bound_check low_x high_x low_y high_y st] is whether the x
    coordinate of [st] is within [low_x]...[high_x] and the y coordinate is
    within [low_y]...[high_y]. Ranges are inclusive on both ends. *)
let button_bound_check low_x high_x low_y high_y st =
  st.mouse_x >= low_x && st.mouse_x <= high_x && st.mouse_y >= low_y
  && st.mouse_y <= high_y

(** [placing_loop g p d] waits for player 1 if [p] is true, player 2 otherwise,
    to press the button for which ship they will place and then allows them to
    place it in game [g] facing direction [d]. [d] being true will place a
    horizontal ship, vertical otherwise. *)
let rec placing_loop game p dir =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  draw_placing_screen game p;
  if st.key == 'q' then quit ()
  else if (* Check for rotation *)
          button_bound_check 680 780 360 410 st then rotate game p dir
  else if (* Check ready button *)
          button_bound_check 680 780 430 480 st then ready game p dir
  else if (* Check reset button *)
          button_bound_check 680 780 700 800 st then reset game p dir
  else if (* Length 5 ship *)
          button_bound_check 21 171 60 100 st then
    ship_placer game p dir carrier
  else if (* Length 4 ship *)
          button_bound_check 181 331 60 100 st then
    ship_placer game p dir destroyer
  else if (* Length 3 ship *)
          button_bound_check 341 491 60 100 st then
    ship_placer game p dir submarine
  else if (* Length 2 ship *)
          button_bound_check 501 651 60 100 st then
    ship_placer game p dir patrol

and rotate game p dir =
  write 295 760 black "Rotate!" 30;
  placing_loop game p (not dir)

and ready game p dir =
  if placed_ready (get_player !game p) then (
    print_endline (string_of_int carrier);
    write 295 760 black "Ready!" 30;
    go_play !game)
  else (
    write 200 760 black "Place all ships to start!" 30;
    placing_loop game p dir)

and reset game p dir =
  if p then
    game :=
      make_game
        (empty_player_board (get_player !game p))
        (get_player !game (not p)) p
  else
    game :=
      make_game (get_player !game (not p))
        (empty_player_board (get_player !game p))
        p;

  write 200 760 black "Click again to reset!" 30;
  placing_loop game p dir

and ship_placer game p dir i =
  let ship_num =
    match i with
    | n when n = carrier -> carrier_num
    | n when n = destroyer -> destroyer_num
    | n when n = submarine -> submarine_num
    | _ -> patrol_num
  in
  if num_placed (get_player !game p) i < ship_num then place_loop game p i dir
  else (
    write 170 760 black ("Max length " ^ string_of_int i ^ " ships on board") 30;
    placing_loop game p dir)

let rec play_loop game p =
  let _ = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  draw_player_board true (get_player !game false)

let main () =
  let _ = open_graph " 800x800" in
  home ();

  while !state = START do
    start_loop game
  done;

  while !state = PLACING do
    placing_loop game true true
  done;

  while !state = PLAY do
    play_loop game true
  done
