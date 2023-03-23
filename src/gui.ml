(* Implement functions from gui.mli *)
open Graphics
open Battleship
open Consts

type state =
  | START
  | PLACING

let state = ref START
let go_green = 0x1B512D
let quit_red = 0x94241a
let logo_wht = 0xF7F7F2
let ocean_blue = 0x7BB5FF
let game = ref (init_game "Player" "AI")

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

(** [write x y c s sz] draws the text of [s] with font size [sz] at pixel
    position ([x],[y]). *)
let write mv_x mv_y color string size =
  set_font
    ("-*-fixed-medium-r-semicondensed--" ^ string_of_int size
   ^ "-*-*-*-*-*-iso8859-1");
  moveto mv_x mv_y;
  set_color color;
  draw_string string

(** [draw_rect c x y w h] draws a rectangle at pixel position ([x],[y]) with
    width [w] and height [h] in color [c]. *)
let draw_rect color x y width height =
  set_color color;
  fill_rect x y width height

(** [home ()] draws the start screen of the game. *)
let home () =
  draw_rect go_green 200 300 400 125;
  write 300 340 white "Start Game" 50;

  draw_rect quit_red 200 100 400 125;
  write 355 140 white "Quit" 50;

  draw_rect black 0 620 800 280;
  write 280 680 white "Battle Ships" 50

(** [draw_cell c x y] draws a cell of color [c] at pixel position ([x],[y]) on
    the grid. *)
let draw_cell color x y =
  draw_rect color
    (background_llx + box_off + (box_size * x))
    (background_tly - box_size - (box_size * y))
    41 41

let quit () = exit 0

(** [draw_player_board p] draws the board associated with player [p]. If the
    board belongs to the current player [self] is true and cells with ships will
    be drawn the same as empty cells. if [self] is false, then draw ship cells
    differently than empty cells. *)
let draw_player_board self p =
  draw_rect black background_llx background_lly background_length
    background_length;
  moveto background_llx background_tly;
  for y = 0 to num_box do
    for x = 0 to num_box do
      match get_coordinate (get_player_board p) (x, y) with
      | Empty t -> draw_cell ocean_blue x y
      | Hit t -> draw_cell quit_red x y
      | Miss t -> draw_cell logo_wht x y
      | Ship t ->
          if self = true then draw_cell green x y else draw_cell ocean_blue x y
    done
  done

(** [go_start g] changes state to PLACING and draws the board of player 1 in
    [g]. *)
let go_start game =
  state := PLACING;
  clear_graph ();
  draw_player_board true (get_player game 1)

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
    draw_rect quit_red 100 20 150 50;
    write 125 35 white "Length 5 ship" 15)
  else if
    (* If condition for quit box *)
    (st.mouse_x >= 200 && st.mouse_x <= 600)
    && st.mouse_y >= 100 && st.mouse_y <= 225
  then quit ()

let rec place_loop game =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  clear_graph ();
  draw_player_board true (get_player !game 1);
  draw_rect quit_red 100 20 150 50;
  write 125 35 white "Length 5 ship" 15;
  let tup = convert st.mouse_x st.mouse_y in
  match tup with
  | None ->
      write 400 35 black "Invalid Position" 30;
      place_loop game
  | Some tup -> (
      try
        place_ship (get_player !game 1) (init_ship 5) (fst tup) (snd tup) 0
        |> draw_player_board true
      with e ->
        write 400 35 black "Invalid Position" 30;
        place_loop game)

let placing_loop game =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  if
    (st.mouse_x >= 100 && st.mouse_x <= 250)
    && st.mouse_y >= 20 && st.mouse_y <= 70
  then place_loop game

let main () =
  let _ = open_graph " 800x800" in
  home ();

  while !state = START do
    start_loop game
  done;

  while !state = PLACING do
    placing_loop game
  done
