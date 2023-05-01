open OUnit2
open Ships.Consts
open Ships.Board
open Ships.Ai
open Ships.Battleship

(********************************************************************
   Helper functions.
 ********************************************************************)

let in_bounds () = []

(********************************************************************
   End helper functions.
 ********************************************************************)

(********************************************************************
  Initilizations
  ********************************************************************)
(* Board inits *)
let board = init_board ()
let ship = ref (init_ship 5)
let ship2 = ref (init_ship 2)

(* Battleships inits *)
let ai = init_player AI
let player = init_player Player
let game = make_game player ai true

(* Ai inits *)
module AI = Make ((val gen_diff_module Hard)) ((val gen_player_module player))

let placed_player = create_placements ship_num_arr player

let board_tests =
  [
    (* init_ship tests: 2*)
    ("init_ship max length test" >:: fun _ -> assert_equal 5 !ship.length);
    ("init_ship min length test" >:: fun _ -> assert_equal 2 !ship2.length);
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
    ( "get_cell after insert non-duplicate" >:: fun _ ->
      assert_equal
        (Ship { ship })
        (let b = board |> insert (2, 2) (Ship { ship }) in
         get_cell b (2, 2)) );
    ( "get_cell after insert duplicate" >:: fun _ ->
      assert_equal
        (Hit { ship })
        (let b =
           board
           |> insert (2, 2) (Ship { ship })
           |> insert (2, 2) (Hit { ship })
         in
         get_cell b (2, 2)) );
    ( "find after insert duplicate" >:: fun _ ->
      assert_equal
        (Some (Ship { ship }))
        (board |> insert (2, 2) (Ship { ship }) |> find (2, 2)) );
    ( "find after insert duplicate" >:: fun _ ->
      assert_equal None (board |> insert (2, 2) (Ship { ship }) |> find (22, 22))
    );
    (* fold tests: 1 *)
    ( "fold for ascending order" >:: fun _ ->
      assert_equal
        ((9, 9), true)
        (board
        |> fold
             (fun (x, y) _ acc -> ((x, y), fst acc @<< (x, y)))
             ((-1, -1), true)) );
    (* to list tests: 1 *)
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

(* AI tests *)
let ai_tests =
  [
    (* gen_diff_module tests: 3 *)
    ( "gen_diff_module is module with difficulty easy" >:: fun _ ->
      let module Diff_mod = (val gen_diff_module Easy) in
      assert_equal Easy Diff_mod.difficulty );
    ( "gen_diff_module is module with difficulty Medium" >:: fun _ ->
      let module Diff_mod = (val gen_diff_module Medium) in
      assert_equal Medium Diff_mod.difficulty );
    ( "gen_diff_module is module with difficulty Hard" >:: fun _ ->
      let module Diff_mod = (val gen_diff_module Hard) in
      assert_equal Hard Diff_mod.difficulty );
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

(* Battleship Tests *)
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
    (* get_player_board: 1 *)
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
  ]

let suite =
  "test suite for final project"
  >::: List.flatten [ board_tests; ai_tests; battleship_tests ]

let _ = run_test_tt_main suite
