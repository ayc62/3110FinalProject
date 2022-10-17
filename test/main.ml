open OUnit2
open Game
open Board
open Check
open Command
open Controller

let board_setup = Board.init_state
let board_tests = []
let kings_pawn = board_setup |> move_piece Pawn White "e2" "e4"

let check_horizontal_test (name : string) (orig_pos : string) (new_pos : string)
    (state : state) (expected_output : bool) =
  "name" >:: fun _ ->
  assert_equal expected_output (check_horizontal orig_pos new_pos state)

let check_vertical_test (name : string) (orig_pos : string) (new_pos : string)
    (state : state) (expected_output : bool) =
  "name" >:: fun _ ->
  assert_equal expected_output (check_vertical orig_pos new_pos state)

let check_diagonal_test (name : string) (orig_pos : string) (new_pos : string)
    (state : state) (expected_output : bool) =
  "name" >:: fun _ ->
  assert_equal expected_output (check_diagonal orig_pos new_pos state)

let check_tests =
  [
    check_horizontal_test "board setup has no pieces b/t a3 and c3" "a3" "b3"
      board_setup true;
    check_horizontal_test "board setup has pieces b/t a2 and c2" "a2" "b2"
      board_setup false;
    check_horizontal_test "board setup has no pieces b/t d5 and h5" "d5" "h5"
      board_setup true;
    check_horizontal_test "kings pawn has pieces b/t a4 and h4" "a4" "h4"
      kings_pawn false;
    check_vertical_test "board setup has no pieces b/t a6 and a3" "a6" "a3"
      board_setup true;
    check_vertical_test "board setup has pieces b/t g8 and g7" "g8" "g7"
      board_setup false;
    check_vertical_test "board setup has pieces b/t a8 and a5" "a8" "a5"
      board_setup false;
    check_vertical_test "board setup has pieces between e5 and e3" "e5" "e3"
      kings_pawn false;
    check_diagonal_test "board setup has no pieces between c3 and a5" "c3" "a5"
      board_setup true;
    check_diagonal_test "board setup has pieces between c3 and h8" "c3" "h8"
      board_setup false;
    check_diagonal_test "board setup has pieces between c3 and e1" "c3" "e1"
      board_setup false;
    check_diagonal_test "board setup has pieces between c3 and a1" "c3" "a1"
      board_setup false;
  ]

let command_tests = []
let controller_tests = []

let suite =
  "chess project test suite"
  >::: List.flatten
         [ board_tests; check_tests; command_tests; controller_tests ]

let _ = run_test_tt_main suite