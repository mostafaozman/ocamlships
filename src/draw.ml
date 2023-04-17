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
  for y = 0 to num_box do
    for x = 0 to num_box do
      match get_cell (get_player_board p) (x, y) with
      | Empty -> draw_cell ocean_blue x y
      | Hit -> draw_cell quit_red x y
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
  draw_rect quit_red 100 20 150 50;
  write 125 35 white "Length 5 ship" 15
