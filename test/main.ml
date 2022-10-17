open OUnit2
open Game
open Board
open Check
open Command
open Controller

let board_tests = []
let check_tests = []
let command_tests = []
let controller_tests = []

let suite =
  "chess project test suite"
  >::: List.flatten
         [ board_tests; check_tests; command_tests; controller_tests ]

let _ = run_test_tt_main suite