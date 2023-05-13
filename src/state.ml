(* Implement functions from state.mli *)
open Graphics
open Consts
open Board
open Battleship
open Ai
open Draw

type state =
  | START
  | INSTRUCTIONS
  | PLACING
  | PLAY
  | PEEK
  | GAMEOVER

let state = ref START
let player = init_player Player
let opp = init_player AI |> create_placements ship_num_arr
let g = make_game player opp true
let game = ref g
let diff = ref Medium
let ai = ref (gen_ai Medium player)

(** [convert x y] is the grid coordinate associated with pixel position
    ([x],[y]). None if coordinate is outside the grid. *)
let convert x y =
  if
    x < background_llx
    || x > background_length + background_llx - box_off
    || y < background_lly || y > background_tly
  then None
  else
    let r = (background_tly - y - box_off) / (box_size + box_off) in
    let c = (x - background_llx - box_off) / (box_size + box_off) in
    Some (c, r)

(** [button_bound_check (low_x,high_x) (low_y,high_y) st] is whether the x
    coordinate of [st] is within [low_x]...[high_x] and the y coordinate is
    within [low_y]...[high_y]. Ranges are inclusive on both ends. *)
let button_bound_check (low_x, high_x) (low_y, high_y) st =
  st.mouse_x >= low_x && st.mouse_x <= high_x && st.mouse_y >= low_y
  && st.mouse_y <= high_y

(** [quit ()] exits the program with exit status 0. *)
let quit () = exit 0

(** [go_start g] changes state to PLACING and draws the board of player 1 in
    [g]. *)
let go_start game =
  state := PLACING;
  clear_graph ();
  draw_player_board true (get_player game true)

let go_instructions game =
  state := INSTRUCTIONS;
  draw_instructions ()

let go_play game =
  state := PLAY;
  clear_graph ();
  draw_fire_screen game

(** [start_loop g] is the start screen of game [g]. *)
let start_loop game =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  if st.key == 'q' then quit ();
  (* If condition for start box *)
  if button_bound_check (200, 600) (300, 425) st then go_instructions game
  else if
    (* If condition for quit box *)
    button_bound_check (200, 600) (100, 225) st
  then quit ()

let instructions_loop game =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  draw_instructions ();
  if st.key == 'q' then quit ();
  (* Check easy button *)
  if button_bound_check (30, 250) (250, 330) st then (
    write 360 350 black "Easy:)" 40;
    diff := Easy);
  (* Check medium button *)
  if button_bound_check (290, 510) (250, 330) st then (
    write 350 350 black "Medium!" 40;
    diff := Medium);
  (* Check hard button *)
  if button_bound_check (550, 770) (250, 330) st then (
    write 360 350 black "Hard>:(" 40;
    diff := Hard);

  (* Check Impossible button *)
  if button_bound_check (290, 510) (150, 230) st then (
    write 300 350 black "Impossible!!!" 40;
    print_endline "Impossible";
    diff := Impossible);

  (* If condition for start box *)
  if button_bound_check (290, 510) (50, 130) st then (
    go_start !game;
    draw_placing_screen (get_curr_player !game))

(** [place_loop g p i] draws the board of player 1 if [p] is true, player 2
    otherwise, after they have placed a ship of length [i] in game [g]. *)
let rec place_loop game i dir =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  draw_placing_screen (get_curr_player !game);
  let tup = convert st.mouse_x st.mouse_y in
  match tup with
  | None ->
      write 200 760 black "Select position on board" 25;
      place_loop game i dir
  | Some tup -> (
      try
        let curr = get_curr_player !game in
        let ship_coords, updated_player =
          place_ship curr (init_ship i) tup dir
        in
        if is_player_1 !game curr then (
          game := make_game updated_player (get_player !game false) true;
          update_cells green ship_coords)
        else game := make_game (get_player !game true) updated_player false;
        update_cells green ship_coords
      with e ->
        write 200 760 black "Can't place ship there" 30;
        place_loop game i dir)

let create_num_remaining_arr p =
  let car_num = carrier_num - num_placed p carrier
  and des_num = destroyer_num - num_placed p destroyer
  and sub_num = submarine_num - num_placed p submarine
  and pat_num = patrol_num - num_placed p patrol in
  [|
    (carrier, car_num);
    (destroyer, des_num);
    (submarine, sub_num);
    (patrol, pat_num);
  |]

(** [placing_loop g p d] waits for player 1 if [p] is true, player 2 otherwise,
    to press the button for which ship they will place and then allows them to
    place it in game [g] facing direction [d]. [d] being true will place a
    horizontal ship, vertical otherwise. *)
let rec placing_loop game dir =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  draw_placing_screen (get_curr_player !game);
  if st.key == 'q' then quit ()
  else if (* Check for rotation *)
          button_bound_check (680, 780) (360, 410) st
  then rotate game dir
  else if (* Check ready button *)
          button_bound_check (680, 780) (430, 480) st
  then ready game dir
  else if (* Check reset button *)
          button_bound_check (680, 780) (700, 800) st
  then reset game dir
  else if (*Auto fill players board*)
          button_bound_check (680, 780) (260, 310) st
  then auto_place game dir
  else if (* Length 5 ship *)
          button_bound_check (21, 171) (60, 100) st then
    ship_placer game dir carrier
  else if (* Length 4 ship *)
          button_bound_check (181, 331) (60, 100) st then
    ship_placer game dir destroyer
  else if (* Length 3 ship *)
          button_bound_check (341, 491) (60, 100) st then
    ship_placer game dir submarine
  else if (* Length 2 ship *)
          button_bound_check (501, 651) (60, 100) st then
    ship_placer game dir patrol

and rotate game dir =
  write 295 760 black "Rotate!" 30;
  placing_loop game (not dir)

and ready game dir =
  let curr_player = get_curr_player !game in
  let curr_is_player_1 = is_player_1 !game curr_player in
  let opp =
    if curr_is_player_1 then get_player !game false else get_player !game true
  in
  let curr_ready, opp_ready = (placed_ready curr_player, placed_ready opp) in
  match (curr_ready, opp_ready) with
  | true, true ->
      write 295 760 black "Ready!" 30;
      if curr_is_player_1 then ai := gen_ai !diff (get_player !game true)
      else ai := gen_ai !diff (get_player !game false);
      go_play game
  | true, false ->
      if curr_is_player_1 then game := make_game curr_player opp false
      else game := make_game opp curr_player true;
      placing_loop game dir
  | _ ->
      write 200 760 black "Place all ships to start!" 30;
      placing_loop game dir

and reset game dir =
  let curr_p = get_curr_player !game in
  begin
    match is_player_1 !game curr_p with
    | true ->
        game :=
          make_game (empty_player_board curr_p) (get_player !game false) true
    | false ->
        game :=
          make_game (get_player !game true) (empty_player_board curr_p) false
  end;
  let curr_p = get_curr_player !game in
  draw_player_board true curr_p;
  placing_loop game dir

and auto_place game dir =
  let curr_p = get_curr_player !game in
  let new_p = create_placements (create_num_remaining_arr curr_p) curr_p in
  begin
    match is_player_1 !game curr_p with
    | true -> game := make_game new_p (get_player !game false) true
    | false -> game := make_game (get_player !game true) new_p false
  end;
  let to_update = get_all_ship_coords new_p in
  update_cells green to_update;
  placing_loop game dir

and ship_placer game dir ship_length =
  let curr_p = get_curr_player !game in
  let ship_num =
    match ship_length with
    | n when n = carrier -> carrier_num
    | n when n = destroyer -> destroyer_num
    | n when n = submarine -> submarine_num
    | _ -> patrol_num
  in
  if num_placed curr_p ship_length < ship_num then
    place_loop game ship_length dir
  else (
    write 170 760 black
      ("Max length " ^ string_of_int ship_length ^ " ships on board")
      30;
    placing_loop game dir)

(** [play_loop g p] allows the player to fire at the opponents board *)
let rec play_loop game =
  let st = wait_next_event [ Button_down; Key_pressed ] in
  synchronize ();
  draw_fire_screen game;
  if st.key == 'q' then quit ()
  else if button_bound_check (680, 780) (400, 460) st then quit ()
  else if
    button_bound_check
      (background_llx, background_llx + background_length)
      (background_lly, background_lly + background_length)
      st
  then try gui_fire game st.mouse_x st.mouse_y with e -> play_loop game
  else play_loop game

and gui_fire game x y =
  let tup = convert x y in
  let shooter = get_curr_player !game in
  let enemy =
    if is_player_1 !game shooter then get_player !game false
    else get_player !game true
  in
  match tup with
  | None -> play_loop game
  | Some (x, y) ->
      let coords, new_opp, res = fire enemy x y in
      (match res with
      | ShipHit -> update_cells quit_red coords
      | ShipMissed -> update_cells logo_wht coords
      | ShipSunk -> update_cells piss_yellow coords);
      if is_game_over new_opp then (
        state := GAMEOVER;
        print_endline "Game Over, You win")
      else game := make_game shooter new_opp true;
      (* AI's turn to shoot *)
      let open (val !ai) in
      let _, new_self, _ = shoot shooter in
      if is_game_over new_self then (
        state := GAMEOVER;
        print_endline "Game Over, AI wins")
      else game := make_game new_self new_opp true

let main () =
  let _ = open_graph " 800x800" in
  home ();

  while !state = START do
    start_loop game
  done;

  while !state = INSTRUCTIONS do
    instructions_loop game
  done;

  while !state = PLACING do
    placing_loop game true
  done;

  while !state = PLAY do
    play_loop game
  done
