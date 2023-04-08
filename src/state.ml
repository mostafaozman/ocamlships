(* Implement functions from gui.mli *)
open Graphics
open Battleship
open Consts
open Draw

type state =
  | START
  | PLACING

let state = ref START
let game = ref (make_game (init_player "Player") (init_player "AI") true)

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
let rec place_loop game p i =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  clear_graph ();
  draw_placing_screen game p;
  let tup = convert st.mouse_x st.mouse_y in
  match tup with
  | None ->
      write 400 35 black "Invalid Position" 30;
      place_loop game p i
  | Some tup -> (
      try
        let updated_p =
          place_ship (get_player !game p) (init_ship i) (fst tup) (snd tup) 0
        in
        if p then (
          game := make_game updated_p (get_player !game (not p)) p;
          draw_player_board true updated_p)
        else game := make_game (get_player !game (not p)) updated_p p;
        draw_player_board true updated_p
      with e ->
        write 400 35 black "Invalid Position" 30;
        place_loop game p i)

(** [placing_loop g p] waits for player 1 if [p] is true, player 2 otherwise, to
    press the button for which ship they will place and then allows them to
    place it in game [g]. *)
let rec placing_loop game p =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  draw_placing_screen game p;
  if
    st.mouse_x >= 100 && st.mouse_x <= 250 && st.mouse_y >= 20
    && st.mouse_y <= 70
  then
    if num_placed (get_player !game p) carrier < 1 then place_loop game p 5
    else (
      write 400 35 black "Max length 5 ships on board" 30;
      placing_loop game p)

let main () =
  let _ = open_graph " 800x800" in
  home ();

  while !state = START do
    start_loop game
  done;

  while !state = PLACING do
    placing_loop game true
  done
