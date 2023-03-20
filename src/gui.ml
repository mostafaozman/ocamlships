(* Implement functions from gui.mli *)
open Graphics
open Battleship

type state =
  | START
  | MyBOARD

let state = ref START
let go_green = 0x1B512D
let quit_red = 0x94241a
let logo_wht = 0xF7F7F2
let ocean_blue = 0x7BB5FF

let write mv_x mv_y color string =
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  moveto mv_x mv_y;
  set_color white;
  draw_string string

let draw_btn color x y width height =
  set_color color;
  fill_rect x y width height

let home () =
  draw_btn go_green 200 300 400 125;
  write 300 340 white "Start Game";

  draw_btn quit_red 200 100 400 125;
  write 355 140 white "Quit";

  draw_btn black 0 620 800 280;
  write 280 680 white "Battle Ships"

let off = 4

let play_board () =
  draw_btn black 70 120 634 634;
  for y = 0 to 13 do
    for x = 0 to 13 do
      draw_btn ocean_blue (70 + off + (45 * x)) (120 + off + (45 * y)) 41 41
    done
  done
(* draw_btn white (70 + off) (120 + off) 41 41; draw_btn white (70 + off + 45)
   (120 + off) 41 41; draw_btn white (70 + off) (120 + off + 45) 41 41 *)
(* let b = init_board () in let rec make_grid b *)

let quit () = exit 0

let go_start () =
  state := MyBOARD;
  clear_graph ();
  play_board ()

let start_game () =
  let _ = open_graph " 800x800" in

  home ();
  while !state = START do
    let st = wait_next_event [ Button_down; Key_pressed ] in
    synchronize ();
    if st.key == 'q' then quit ();
    (* If condition for start box *)
    if
      (st.mouse_x >= 200 && st.mouse_x <= 600)
      && st.mouse_y >= 300 && st.mouse_y <= 425
    then go_start () (* If condition for quit box *)
    else if
      (st.mouse_x >= 200 && st.mouse_x <= 600)
      && st.mouse_y >= 100 && st.mouse_y <= 225
    then quit ()
  done;

  while !state = MyBOARD do
    ()
  done
