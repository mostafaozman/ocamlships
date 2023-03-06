open OUnit2
open Battleship

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

let example_test_function (name : string) (input : char) (expected_output : int)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (int_of_char input) ~printer:string_of_int

(********************************************************************
   End helper functions.
 ********************************************************************)

(* example test suites *)
let adventure_tests = [ example_test_function "name" 'A' 0 ]
let command_tests = []
let state_tests = []

let suite =
  "test suite for final project"
  >::: List.flatten [ adventure_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite