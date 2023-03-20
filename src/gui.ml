(* Implement functions from gui.mli *)
open Graphics

let go_green = 0x1B512D
let quit_red = 0x94241a
let logo_wht = 0xF7F7F2

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

let quit () = exit 0
let go_start () = clear_graph ()

let start_game () =
  let _ = open_graph " 800x800" in
  let state = true in

  home ();

  while state do
    let st = wait_next_event [ Button_down; Key_pressed ] in
    synchronize ();
    if st.key == 'q' then quit ();
    (* If comdition for start box *)
    if
      (st.mouse_x >= 200 && st.mouse_x <= 600)
      && st.mouse_y >= 300 && st.mouse_y <= 425
    then go_start () (* If comdition for quit box *)
    else if
      (st.mouse_x >= 200 && st.mouse_x <= 600)
      && st.mouse_y >= 100 && st.mouse_y <= 225
    then quit ()
  done
