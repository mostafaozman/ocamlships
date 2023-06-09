open Graphics
open Consts
open Battleship
open Board
module Str = Str

let write mv_x mv_y color string size =
  set_font
    ("-*-fixed-medium-r-semicondensed--" ^ string_of_int size
   ^ "-*-*-*-*-*-iso8859-1");
  moveto mv_x mv_y;
  set_color color;
  draw_string string

let draw_rect color x y width height =
  set_color color;
  fill_rect x y width height

let draw_cell color x y =
  draw_rect color
    (background_llx + box_off + ((box_size + box_off) * x))
    (background_tly - (box_size + box_off) - ((box_size + box_off) * y))
    box_size box_size

(** [parameterize_string s f p] is [s] with all substring that match the first
    element of a tuple in [p] replaced by the second element of that tuple after
    it has been converted to a string by [f]. *)
let parameterize_string s f pair_list =
  List.fold_left
    (fun acc (to_match, elem) ->
      let regex = Str.regexp_string to_match in
      Str.global_replace regex (f elem) acc)
    s pair_list

let draw_instructions () =
  let you_must =
    "- You must place: %nc len %c, %nd len %d, %ns len %s, and %np len %p"
  in
  let paramerized_string =
    parameterize_string you_must string_of_int
      [
        ("%nc", carrier_num);
        ("%c", carrier);
        ("%nd", destroyer_num);
        ("%d", destroyer);
        ("%ns", submarine_num);
        ("%s", submarine);
        ("%np", patrol_num);
        ("%p", patrol);
      ]
  in
  draw_rect white 0 0 800 800;
  draw_rect go_green 290 50 220 80;
  write 325 70 white "Continue" 40;
  write 230 730 black "Instructions" 60;
  write 30 680 black
    "- Place ships by clicking on a ship then clicking a square on" 25;
  write 52 655 black
    "the board where you would like the center of the ship to be" 25;
  write 30 625 black "- Ships are placed horizontally by default" 25;
  write 30 595 black "- Rotate a ship by clicking the rotate button then a ship"
    25;
  write 30 565 black paramerized_string 25;
  write 30 535 black
    "- After placing ships, click the ready button to start the game" 25;
  write 30 505 black
    "- To continue, choose a difficulty then select the button below" 25;
  write 30 475 black "- Difficulty is MEDIUM by default if you don't choose" 25;
  draw_rect go_green 30 250 220 80;
  write 100 270 white "Easy" 40;
  draw_rect piss_yellow 290 250 220 80;
  write 355 270 white "Medium" 40;
  draw_rect quit_red 550 250 220 80;
  write 625 270 white "Hard" 40;
  draw_rect purple 290 150 220 80;
  write 300 170 white "Impossible" 40

let draw_game_over game b =
  draw_rect black 0 0 800 800;
  draw_rect go_green 290 50 220 80;
  write 310 70 white "Play Again" 40;
  draw_rect quit_red 290 150 220 80;
  write 370 170 white "Quit" 40;
  if b then write 110 450 red "PLAYER ONE WINS!" 80
  else write 110 450 red "PLAYER TWO WINS!" 80

let draw_player_board self p =
  draw_rect black background_llx background_lly background_length
    background_length;
  moveto background_llx background_tly;
  let board = get_player_board p in
  for y = 0 to board_size - 1 do
    for x = 0 to board_size - 1 do
      match get_cell board (x, y) with
      | Empty -> draw_cell ocean_blue x y
      | Hit _ -> draw_cell quit_red x y
      | Sunk _ -> draw_cell piss_yellow x y
      | Miss -> draw_cell logo_wht x y
      | Ship _ ->
          if self = true then draw_cell green x y else draw_cell ocean_blue x y
    done
  done

let draw_peek self p last =
  clear_graph ();
  write 230 755 black "Your Board" 35;
  draw_player_board self p;

  write 695 554 black "Key:" 40;

  draw_rect black 680 464 50 50;
  draw_rect piss_yellow 685 469 40 40;
  write 735 474 black "Sunk" 30;

  draw_rect black 680 384 50 50;
  draw_rect quit_red 685 389 40 40;
  write 735 394 black "Hit" 30;

  draw_rect black 680 304 50 50;
  draw_rect white 685 309 40 40;
  write 735 314 black "Miss" 30;

  draw_rect black 680 224 50 50;
  draw_rect white 685 229 40 40;
  write 695 225 purple "X" 50;
  write 735 248 black "Last" 20;
  write 735 228 black "Hit" 20;

  begin
    match last with
    | None -> ()
    | Some (x, y) ->
        write
          (background_llx + box_off
          + ((box_size + box_off) * x)
          + (box_size / 3) - 5)
          (background_tly - (box_size + box_off)
          - ((box_size + box_off) * y)
          - (box_size / 3) + 5)
          purple "X" 85
  end;

  draw_rect quit_red 600 20 175 60;
  write 610 30 white "< Back" 40

let home () =
  draw_rect go_green 200 300 400 125;
  write 300 340 white "Start Game" 50;

  draw_rect quit_red 200 100 400 125;
  write 355 140 white "Quit" 50;
  draw_rect black 0 620 800 280;
  write 280 680 white "Battle Ships" 50

let draw_placing_screen player =
  draw_player_board true player;
  (* Ready button *)
  draw_rect white 0 755 800 45;
  draw_rect go_green 680 430 100 50;
  write 705 448 white "Ready" 20;
  (* Reset button *)
  draw_rect depression_grey 680 700 100 50;
  write 705 715 white "Reset" 20;
  (* Rotate button *)
  draw_rect piss_yellow 680 360 100 50;
  write 700 378 white "Rotate" 20;
  (* Auto place button *)
  draw_rect purple 680 260 100 50;
  write 685 270 white "Auto-place" 19;

  draw_rect quit_red 21 60 150 40;
  write 46 74 white "Length 5 ship" 15;
  draw_rect quit_red 181 60 150 40;
  write 206 74 white "Length 4 ship" 15;
  draw_rect quit_red 341 60 150 40;
  write 366 74 white "Length 3 ship" 15;

  draw_rect quit_red 501 60 150 40;
  write 526 74 white "Length 2 ship" 15;

  write 180 20 black "Direction: " 30;
  write 340 20 black horiz 30

let draw_fire_screen game =
  let curr_player = get_curr_player !game in
  begin
    match is_player_1 !game curr_player with
    | true -> draw_player_board false (get_player !game false)
    | false -> draw_player_board false (get_player !game true)
  end;

  draw_rect quit_red 680 694 100 60;
  write 695 704 white "Quit" 40;

  write 695 554 black "Key:" 40;

  draw_rect black 680 464 50 50;
  draw_rect piss_yellow 685 469 40 40;
  write 735 474 black "Sunk" 30;

  draw_rect black 680 384 50 50;
  draw_rect quit_red 685 389 40 40;
  write 735 394 black "Hit" 30;

  draw_rect black 680 304 50 50;
  draw_rect white 685 309 40 40;
  write 735 314 black "Miss" 30;

  write 325 50 black "Fire!" 40;

  draw_rect go_green 600 20 175 60;
  write 650 25 white "Peek" 40;

  write 230 755 black "AI's Board" 35

let rec update_cells color lst =
  match lst with
  | [] -> ()
  | (x, y) :: t ->
      draw_cell color x y;
      update_cells color t
