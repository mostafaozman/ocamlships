(* Implement functions from gui.mli *)
open Graphics

let go_green = 0x1B512D
let quit_red = 0x94241a
let logo_wht = 0xF7F7F2

let write size mv_x mv_y color string =
  set_text_size size;
  moveto mv_x mv_y;
  set_color white;
  draw_string string

let draw_btn color x y width height =
  set_color color;
  fill_rect x y width height

let home () =
  draw_btn go_green 200 300 400 125;

  write 20 375 350 white "Start Game";
  draw_btn quit_red 200 100 400 125;

  write 20 390 150 white "Quit";
  draw_btn black 0 620 800 280;
  write 50 370 700 white "Battle Ships"

let start_game () =
  let _ = open_graph " 800x800" in
  let state = true in

  home ();

  while state do
    ()
  done

let quit () = close_graph ()
