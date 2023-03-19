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
  draw_btn go_green 150 300 500 100;
  write 20 450 350 white "Start Game";

  draw_btn quit_red 150 100 375 100;
  write 20 450 150 white "Quit";

  draw_btn black 170 620 600 280;
  write 50 150 700 white "Battle Ships"

let start_game () =
  let _ = open_graph " 800x800" in
  let state = true in

  home ();

  while state do
    ()
  done

let quit () = close_graph ()
