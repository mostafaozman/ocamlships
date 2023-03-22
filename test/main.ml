open OUnit2
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

(* example test suites *)
let board = init_board

let init_tests =
  [
    ( "Initialize ship test" >:: fun _ ->
      assert_equal 4 (init_ship 4 |> get_ship_length) );
  ]

let getter_tests = []
let func_test = []

let suite =
  "test suite for final project"
  >::: List.flatten [ init_tests; getter_tests; func_test ]

let _ = run_test_tt_main suite
