(* Implement functions from gui.mli *)
open GMain
open Graphics

let go_green = 0x1B512D
let quit_red = 0x94241a
let logo_wht = 0xF7F7F2

(* let _ = GMain.init ()

   let main () = let window = GWindow.window ~border_width:800
   ~title:"LablGL/Gtk" () in window#connect#destroy ~callback:Main.quit; ()

   let button = GButton.button ~label:"Hello World" ~packing:window#add ()
   window#show (); GMain.main ()

   (* let add_buttons ?pixfile canvas = let group = canvas#get_root_item in let
   start = GooCanvas.rect ~props: [X 10; Y 10; ] *) *)
let start_game () =
  let state = true in
  let _ = open_graph " 800x800" in

  let draw_btn color x y width height =
    set_color color;
    fill_rect x y width height
  in

  draw_btn go_green 150 300 500 100;
  moveto 150 300;
  draw_string "Start Game";

  draw_btn quit_red 150 100 375 100;
  moveto 150 100;
  draw_string "Quit";

  draw_btn black 170 620 600 280;
  moveto 150 600;

  draw_string "Battle Ship";

  while state do
    ()
  done

let quit () = close_graph ()
