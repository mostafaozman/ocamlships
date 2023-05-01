open OUnit2
open Ships.Board
open Ships.Battleship

(********************************************************************
   Helper functions.
 ********************************************************************)

let init_board_test (name : string) (input : char) (expected_output : int) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (int_of_char input) ~printer:string_of_int

(********************************************************************
   End helper functions.
 ********************************************************************)

(* Initializations *)
let board = init_board ()
let ship = ref (init_ship 5)
let ship2 = ref (init_ship 2)
let ai = init_player AI
let player = init_player Player
let game = make_game player ai true

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
      assert_equal (Some Empty) (board |> find (13, 13)) );
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
        ((13, 13), true)
        (board
        |> fold
             (fun (x, y) _ acc -> ((x, y), fst acc @<< (x, y)))
             ((-1, -1), true)) );
    (* to list tests: 1 *)
    ( "to list initial board" >:: fun _ ->
      assert_equal
        (fold (fun (x, y) cell acc -> ((x, y), cell) :: acc) [] board)
        (board |> to_list |> List.rev) )
    (* ( "to list with ships inserted" >:: fun _ -> assert_equal (fold (fun (x,
       y) cell acc -> ((x, y), cell) :: acc) [] board) (board |> insert (2, 2)
       Miss |> to_list |> List.rev) ); *);
  ]

(* AI tests *)
let ai_tests = []

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
      assert_equal Empty (get_cell board (13, 13)) );
    ( "get_cell (-1,-1) raises failure" >:: fun _ ->
      assert_raises (InvalidPosition "(-1,-1)") (fun () ->
          get_cell board (-1, -1)) );
  ]

let suite =
  "test suite for final project"
  >::: List.flatten [ board_tests; ai_tests; battleship_tests ]

let _ = run_test_tt_main suite
