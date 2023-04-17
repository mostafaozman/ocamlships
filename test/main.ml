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

(* example test suites *)
let board = init_board ()
let board_tests = []
let battleship_tests = []
let suite = "test suite for final project" >::: List.flatten []
let _ = run_test_tt_main suite
