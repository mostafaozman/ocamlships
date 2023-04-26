open Graphics
open Consts
open Battleship
open Board

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
    (background_llx + box_off + (box_size * x))
    (background_tly - box_size - (box_size * y))
    41 41

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

let home () =
  draw_rect go_green 200 300 400 125;
  write 300 340 white "Start Game" 50;

  draw_rect quit_red 200 100 400 125;
  write 355 140 white "Quit" 50;
  draw_rect black 0 620 800 280;
  write 280 680 white "Battle Ships" 50

let draw_placing_screen game player =
  draw_player_board true (get_player !game player);
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

  draw_rect quit_red 21 60 150 40;
  write 46 74 white "Length 5 ship" 15;
  draw_rect quit_red 181 60 150 40;
  write 206 74 white "Length 4 ship" 15;
  draw_rect quit_red 341 60 150 40;
  write 366 74 white "Length 3 ship" 15;

  draw_rect quit_red 501 60 150 40;
  write 526 74 white "Length 2 ship" 15

let rec update_cells self lst =
  match lst with
  | [] -> ()
  | (x, y) :: t ->
      if self = true then draw_cell green x y else draw_cell ocean_blue x y;
      update_cells self t
