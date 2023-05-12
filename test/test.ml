open OUnit2
open Ships.Consts
open Ships.Board
open Ships.Ai
open Ships.Battleship

(********* Test Plan: *********)

(*********** Initilizations ********)
(* Board inits *)
let board = init_board ()
let ship = ref (init_ship 5)
let ship2 = ref (init_ship 3)

(* Battleships inits *)
let ai = init_player AI
let player = init_player Player
let game = make_game player ai true

(* Ai inits *)
module AI = Make ((val gen_diff_module Hard)) ((val gen_player_module player))

let placed_player = create_placements ship_num_arr player
let coords, player_w_ship_horz = place_ship player !ship2 (3, 2) true
let coords2, player_w_ship_vert = place_ship player !ship2 (3, 2) false

(* Board tests: 26 *)
let board_tests =
  [
    (*@<< tests: 5*)
    ( "(2,2) @<< (2,2) is false" >:: fun _ ->
      assert_equal false ((2, 2) @<< (2, 2)) );
    ( "(2,5) @<< (2,2) is false" >:: fun _ ->
      assert_equal false ((2, 5) @<< (2, 2)) );
    ( "(5,2) @<< (2,2) is false" >:: fun _ ->
      assert_equal false ((5, 2) @<< (2, 2)) );
    ( "(2,2) @<< (2,5) is true" >:: fun _ ->
      assert_equal true ((2, 2) @<< (2, 5)) );
    ( "(2,2) @<< (5,2) is true" >:: fun _ ->
      assert_equal true ((2, 2) @<< (5, 2)) );
    (*>>@ tests: 5*)
    ( "(2,2) >>@  (2,2) is false" >:: fun _ ->
      assert_equal false ((2, 2) >>@ (2, 2)) );
    ( "(2,5) >>@ (2,2) is true" >:: fun _ ->
      assert_equal true ((2, 5) >>@ (2, 2)) );
    ( "(5,2) >>@ (2,2) is true" >:: fun _ ->
      assert_equal true ((5, 2) >>@ (2, 2)) );
    ( "(2,2) >>@ (2,5) is false" >:: fun _ ->
      assert_equal false ((2, 2) >>@ (2, 5)) );
    ( "(2,2) >>@ (5,2) is false" >:: fun _ ->
      assert_equal false ((2, 2) >>@ (5, 2)) );
    (* init_ship tests: 2*)
    ("init_ship max length test" >:: fun _ -> assert_equal 5 !ship.length);
    ("init_ship min length test" >:: fun _ -> assert_equal 3 !ship2.length);
    (* string_of_cords tests: 3 *)
    ( "string_of_cords (5,2) is \"(5,2)\"" >:: fun _ ->
      assert_equal "(5,2)" (string_of_coord (5, 2)) );
    ( "string_of_cords (0,0) is \"(0,0)\"" >:: fun _ ->
      assert_equal "(0,0)" (string_of_coord (0, 0)) );
    ( "string_of_cords (14,14) is \"(14,14)\"" >:: fun _ ->
      assert_equal "(14,14)" (string_of_coord (14, 14)) );
    (*Find test: 4 *)
    ( "find on empty board is empty" >:: fun _ ->
      assert_equal (Some Empty) (board |> find (2, 2)) );
    ( "find lower bound is Some Empty" >:: fun _ ->
      assert_equal (Some Empty) (board |> find (0, 0)) );
    ( "find upper bound is Some Empty" >:: fun _ ->
      assert_equal (Some Empty) (board |> find (board_size - 1, board_size - 1))
    );
    ( "find out of bounds is None" >:: fun _ ->
      assert_equal None (board |> find (22, 22)) );
    (*insert tests: 4 *)
    ( "insert non-duplicate then get_cell is Ship" >:: fun _ ->
      assert_equal
        (Ship { ship })
        (let b = board |> insert (2, 2) (Ship { ship }) in
         get_cell b (2, 2)) );
    ( "insert duplicate then get_cell is Hit" >:: fun _ ->
      assert_equal
        (Hit { ship })
        (let b =
           board
           |> insert (2, 2) (Ship { ship })
           |> insert (2, 2) (Hit { ship })
         in
         get_cell b (2, 2)) );
    ( "insert non-dup then find is Some Ship" >:: fun _ ->
      assert_equal
        (Some (Ship { ship }))
        (board |> insert (2, 2) (Ship { ship }) |> find (2, 2)) );
    ( "insert duplicate then find is Empty" >:: fun _ ->
      assert_equal (Some Empty)
        (board
        |> insert (2, 2) (Ship { ship })
        |> insert (2, 2) Empty
        |> find (2, 2)) );
    (* fold tests: 1 *)
    ( "fold for ascending order" >:: fun _ ->
      assert_equal
        ((board_size - 1, board_size - 1), true)
        (board
        |> fold
             (fun (x, y) _ acc -> ((x, y), fst acc @<< (x, y)))
             ((-1, -1), true)) );
    (* to list tests: 2 *)
    ( "to list initial board" >:: fun _ ->
      assert_equal
        (fold (fun (x, y) cell acc -> ((x, y), cell) :: acc) [] board)
        (board |> to_list |> List.rev) );
    ( "to list with ships inserted" >:: fun _ ->
      assert_equal
        (fold
           (fun (x, y) cell acc -> ((x, y), cell) :: acc)
           []
           (board |> insert (2, 2) Miss))
        (board |> insert (2, 2) Miss |> to_list |> List.rev) );
  ]

(* AI tests: 6 *)
let ai_tests =
  [
    (* gen_diff_module tests: 4 *)
    ( "gen_diff_module is module with difficulty easy" >:: fun _ ->
      let module Diff_mod = (val gen_diff_module Easy) in
      assert_equal Easy Diff_mod.difficulty );
    ( "gen_diff_module is module with difficulty Medium" >:: fun _ ->
      let module Diff_mod = (val gen_diff_module Medium) in
      assert_equal Medium Diff_mod.difficulty );
    ( "gen_diff_module is module with difficulty Hard" >:: fun _ ->
      let module Diff_mod = (val gen_diff_module Hard) in
      assert_equal Hard Diff_mod.difficulty );
    ( "gen_diff_module is module with difficulty Hard" >:: fun _ ->
      let module Diff_mod = (val gen_diff_module Impossible) in
      assert_equal Impossible Diff_mod.difficulty );
    (* gen_player_module tests: 1 *)
    ( "gen_player_module is a module with player of type player" >:: fun _ ->
      let module Player_mod = (val gen_player_module player) in
      assert_equal player Player_mod.player );
    (* create_placements tests : 1 *)
    ( "create_placements has 7 ships placed" >:: fun _ ->
      assert_equal true (placed_player |> placed_ready) )
    (* Ai shoot : 1 *)
    (* ( "ArtIntelligence.shoot returns a tuple " >:: fun _ -> let t, _, _ =
       AI.shoot placed_player in assert_equal t ); *);
  ]

(* Battleship Tests: 27 *)
let battleship_tests =
  [
    (* get_player tests: 2 *)
    ( "Player1 is player test" >:: fun _ ->
      assert_equal player (get_player game true) );
    ("Player2 is ai test" >:: fun _ -> assert_equal ai (get_player game false));
    (* empty_player_board test: 1 *)
    ( "players board is empty" >:: fun _ ->
      assert_equal board
        (snd (place_ship (init_player Player) !ship (2, 2) true)
        |> empty_player_board |> get_player_board) );
    (* get_player_board tests: 1 *)
    ( "players board is empty" >:: fun _ ->
      assert_equal board (get_player_board player) );
    (* get_cells tests: 4 *)
    ( "get_cell on empty board is empty" >:: fun _ ->
      assert_equal Empty (get_cell board (2, 2)) );
    ( "get_cell lower bound is empty" >:: fun _ ->
      assert_equal Empty (get_cell board (0, 0)) );
    ( "get_cell upper bound is empty" >:: fun _ ->
      assert_equal Empty (get_cell board (board_size - 1, board_size - 1)) );
    ( "get_cell (-1,-1) raises failure" >:: fun _ ->
      assert_raises (InvalidPosition "(-1,-1)") (fun () ->
          get_cell board (-1, -1)) );
    (* get_adjacent_of_point tests: 2*)
    ( "adjacent of (0,0) is only [(0,1);(1,0)]" >:: fun _ ->
      assert_equal [ (0, 1); (1, 0) ] (get_adjacents_of_point (0, 0)) );
    ( "adjacent of (5,5) is only [(5,4);(4,5);(5,6);(6,5)]" >:: fun _ ->
      assert_equal
        [ (5, 4); (4, 5); (5, 6); (6, 5) ]
        (get_adjacents_of_point (5, 5)) );
    (* pp tests: 2*)
    ("pp empty list is []" >:: fun _ -> assert_equal "[]" (pp []));
    ( "pp [(10,10);(0,0);(5,3)] list is \"[(10,10);(0,0);(5,3)]\"" >:: fun _ ->
      assert_equal "[(10,10);(0,0);(5,3)]" (pp [ (10, 10); (0, 0); (5, 3) ]) );
    (* num_placed tests: 3 *)
    ( "num_placed on empty board is zero" >:: fun _ ->
      assert_equal 0 (num_placed player 3) );
    ( "num_placed on board with one submarine is 1" >:: fun _ ->
      assert_equal 1 (num_placed player_w_ship_horz 3) );
    ( "num_placed with invalid ship length is 0" >:: fun _ ->
      assert_equal 0 (num_placed player_w_ship_horz 7) );
    (* place_ship tests: 3 *)
    ( "place_ship horizontal submarine in (3,2)" >:: fun _ ->
      assert_equal
        ([ (4, 2); (3, 2); (2, 2) ], Ship { ship = ship2 })
        (let board = player_w_ship_horz |> get_player_board in
         (coords, get_cell board (3, 2))) );
    ( "place_ship vertical submarine in (3,2)" >:: fun _ ->
      assert_equal
        ([ (3, 1); (3, 2); (3, 3) ], Ship { ship = ship2 })
        (let board = player_w_ship_vert |> get_player_board in
         (coords2, get_cell board (3, 2))) );
    ( "place_ship on non empty cell is InvalidPosition" >:: fun _ ->
      assert_raises (InvalidPosition "") (fun () ->
          place_ship player_w_ship_horz (init_ship 3) (3, 2) true) );
    ( "place_ship on out of bounds is InvalidPosition" >:: fun _ ->
      assert_raises (InvalidPosition "(10,11)") (fun () ->
          place_ship player_w_ship_horz (init_ship 3) (11, 11) true) );
    (* fire tests: *)

    (* possible_place tests: 4 *)
    ( "possible_place horizontal carrier in (3,2)" >:: fun _ ->
      assert_equal
        [ (7, 6); (6, 6); (5, 6); (4, 6); (3, 6) ]
        (possible_place player (init_ship 5) (5, 6) true) );
    ( "possible_place vertical carrier in (3,2)" >:: fun _ ->
      assert_equal
        [ (5, 8); (5, 7); (5, 6); (5, 5); (5, 4) ]
        (possible_place player (init_ship 5) (5, 6) false) );
    ( "possible_place on non empty cell is InvalidPosition" >:: fun _ ->
      assert_raises (InvalidPosition "") (fun () ->
          possible_place player_w_ship_horz (init_ship 3) (3, 2) true) );
    ( "possible_place on out of bounds is InvalidPosition" >:: fun _ ->
      assert_raises (InvalidPosition "(10,11)") (fun () ->
          possible_place player_w_ship_horz (init_ship 3) (11, 11) true) );
    (*placed_ready test: 2*)
    ( "placed_ready with all placed is true" >:: fun _ ->
      assert_equal true (placed_player |> placed_ready) );
    ( "placed_ready with none placed is false" >:: fun _ ->
      assert_equal false (player |> placed_ready) )
    (* is_game_over test: 2 *);
    ( "is_game_over with none placed is true" >:: fun _ ->
      assert_equal true (player |> is_game_over) );
    ( "is_game_over with some placed is true" >:: fun _ ->
      assert_equal false (placed_player |> is_game_over) );
  ]

let suite =
  "test suite for final project"
  >::: List.flatten [ board_tests; ai_tests; battleship_tests ]

let _ = run_test_tt_main suite
