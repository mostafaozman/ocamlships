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
let ship = ref (init_ship 4)

let board_tests =
  [
    (* HOW TEST BOARD IDKKKK *)
    (* ("init_board test" >:: fun _ -> assert_equal 2 board.); *)
    (* init_ship tests: 2*)
    ("init_ship length test" >:: fun _ -> assert_equal 4 !ship.length);
    ("init_ship adjacents test" >:: fun _ -> assert_equal [] !ship.adjacents);
    (* string_of_cords tests: 3 *)
    ( "string_of_cords (5,2) is \"(5,2)\"" >:: fun _ ->
      assert_equal "(5,2)" (string_of_coord (5, 2)) );
    ( "string_of_cords (0,0) is \"(0,0)\"" >:: fun _ ->
      assert_equal "(0,0)" (string_of_coord (0, 0)) );
    ( "string_of_cords (14,14) is \"(14,14)\"" >:: fun _ ->
      assert_equal "(14,14)" (string_of_coord (14, 14)) );
    (* get_cells tests: 4 *)
    ( "get_cell empty board is empty" >:: fun _ ->
      assert_equal Empty (get_cell board (2, 2)) );
    ( "get_cell lower bound is empty" >:: fun _ ->
      assert_equal Empty (get_cell board (0, 0)) );
    ( "get_cell upper bound is empty" >:: fun _ ->
      assert_equal Empty (get_cell board (14, 14)) );
    ( "get_cell (-1,-1) raises failure" >:: fun _ ->
      assert_raises (InvalidPosition "(-1,-1)") (fun () ->
          get_cell board (-1, -1)) );
    (*insert tests: 2 *)
    ( "get_cell after insert non-duplicate" >:: fun _ ->
      assert_equal
        (Ship { ship })
        (let b = board |> insert (2, 2) (Ship { ship }) in
         get_cell b (2, 2)) );
    ( "get_cell after insert duplicate" >:: fun _ ->
      assert_equal Hit
        (let b = board |> insert (2, 2) (Ship { ship }) |> insert (2, 2) Hit in
         get_cell b (2, 2)) );
  ]

(* AI tests *)
let ai_tests = []

(* Battleship Tests *)
let battleship_tests = []

let suite =
  "test suite for final project"
  >::: List.flatten [ board_tests; ai_tests; battleship_tests ]

let _ = run_test_tt_main suite
