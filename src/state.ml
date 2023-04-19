(* Implement functions from state.mli *)
open Graphics
open Consts
open Board
open Battleship
open Draw

type state =
  | START
  | PLACING
  | PLAY

let num_carrier = ref 0
let num_destroyer = ref 0
let num_submarine = ref 0
let state = ref START
let player_board = init_player "Player"
let op_board = init_player "AI"
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
      write 200 760 black "Invalid Position 1" 30;
      place_loop game p i dir
  | Some tup -> (
      try
        let updated_p =
          place_ship (get_player !game p) (init_ship i) (fst tup) (snd tup) dir
        in
        if p then (
          game := make_game (snd updated_p) (get_player !game (not p)) p;
          update_cells true (fst updated_p))
        else game := make_game (get_player !game (not p)) (snd updated_p) p;
        update_cells true (fst updated_p)
      with e ->
        write 200 760 black "Invalid Position 2" 30;
        place_loop game p i dir)

(** [placing_loop g p] waits for player 1 if [p] is true, player 2 otherwise, to
    press the button for which ship they will place and then allows them to
    place it in game [g]. *)
let rec placing_loop game p dir =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  draw_placing_screen game p;
  if st.key == 'q' then quit ();
  (*Check for rotation*)
  if
    st.mouse_x >= 680 && st.mouse_x <= 780 && st.mouse_y >= 360
    && st.mouse_y <= 410
  then (
    write 295 760 black "Rotate!" 30;
    placing_loop game p (not dir));
  (* Check ready button *)
  if
    st.mouse_x >= 680 && st.mouse_x <= 780 && st.mouse_y >= 430
    && st.mouse_y <= 480
  then
    if !num_carrier = 2 && !num_destroyer = 2 && !num_submarine = 2 then (
      print_endline (string_of_int carrier);
      write 295 760 black "Ready!" 30;
      go_play !game)
    else (
      write 200 760 black "Place all ships to start!" 30;
      placing_loop game p dir);
  (* Check reset button *)
  if
    st.mouse_x >= 680 && st.mouse_x <= 780 && st.mouse_y >= 700
    && st.mouse_y <= 800
  then (
    game := make_game (init_player "Player") op_board true;
    write 200 760 black "Click again to reset!" 30;
    num_carrier := 0;
    num_destroyer := 0;
    num_submarine := 0;
    placing_loop game p dir);
  (* Length 5 ship *)
  if
    st.mouse_x >= 21 && st.mouse_x <= 171 && st.mouse_y >= 10
    && st.mouse_y <= 50
    || st.mouse_x >= 21 && st.mouse_x <= 171 && st.mouse_y >= 60
       && st.mouse_y <= 100
  then
    if num_placed (get_player !game p) carrier < carrier_num then
      place_loop game p 5 dir
    else (
      write 170 760 black "Max length 5 ships on board" 30;
      placing_loop game p dir);
  (* Length 4 ship *)
  if
    st.mouse_x >= 181 && st.mouse_x <= 331 && st.mouse_y >= 10
    && st.mouse_y <= 50
    || st.mouse_x >= 181 && st.mouse_x <= 331 && st.mouse_y >= 60
       && st.mouse_y <= 100
  then
    if num_placed (get_player !game p) destroyer < destroyer_num then
      place_loop game p 4 dir
    else (
      write 170 760 black "Max length 4 ships on board" 30;
      placing_loop game p dir);
  (* Length 3 ship *)
  if
    st.mouse_x >= 341 && st.mouse_x <= 491 && st.mouse_y >= 10
    && st.mouse_y <= 50
    || st.mouse_x >= 341 && st.mouse_x <= 491 && st.mouse_y >= 60
       && st.mouse_y <= 100
  then
    if num_placed (get_player !game p) submarine < submarine_num then
      place_loop game p 3 dir
    else (
      write 170 760 black "Max length 3 ships on board" 30;
      placing_loop game p dir);
  (* Length 2 ship *)
  if
    st.mouse_x >= 501 && st.mouse_x <= 651 && st.mouse_y >= 10
    && st.mouse_y <= 50
    || st.mouse_x >= 501 && st.mouse_x <= 651 && st.mouse_y >= 60
       && st.mouse_y <= 100
  then
    if num_placed (get_player !game p) patrol < patrol_num then
      place_loop game p 2 dir
    else (
      write 170 760 black "Max length 2 ships on board" 30;
      placing_loop game p dir)

let main () =
  let _ = open_graph " 800x800" in
  home ();

  while !state = START do
    start_loop game
  done;

  while !state = PLACING do
    placing_loop game true true
  done
