open OUnit2
open Game
open Board
open Check
open Command
open Controller

let board_setup = Board.init_state
let board_tests = []
let kings_pawn = move_piece Pawn White "e2" "e4" board_setup []

let is_horizontal_test (name : string) (orig_pos : string) (new_pos : string)
    (expected_output : bool) =
  "name" >:: fun _ ->
  assert_equal expected_output (is_horizontal orig_pos new_pos)

let check_horizontal_test (name : string) (orig_pos : string) (new_pos : string)
    (state : state) (expected_output : bool) =
  "name" >:: fun _ ->
  assert_equal expected_output (check_horizontal orig_pos new_pos state)

let is_vertical_test (name : string) (orig_pos : string) (new_pos : string)
    (expected_output : bool) =
  "name" >:: fun _ ->
  assert_equal expected_output (is_vertical orig_pos new_pos)

let check_vertical_test (name : string) (orig_pos : string) (new_pos : string)
    (state : state) (expected_output : bool) =
  "name" >:: fun _ ->
  assert_equal expected_output (check_vertical orig_pos new_pos state)

let is_diagonal_test (name : string) (orig_pos : string) (new_pos : string)
    (expected_output : bool) =
  "name" >:: fun _ ->
  assert_equal expected_output (is_diagonal orig_pos new_pos)

let check_diagonal_test (name : string) (orig_pos : string) (new_pos : string)
    (state : state) (expected_output : bool) =
  "name" >:: fun _ ->
  assert_equal expected_output (check_diagonal orig_pos new_pos state)

let check_tests =
  [
    is_horizontal_test "a3 and g3 are in the same row" "a3" "g3" true;
    is_horizontal_test "a3 and c5 are not in the same row" "a3" "c5" false;
    check_horizontal_test "board setup has no pieces b/t a3 and c3" "a3" "b3"
      board_setup true;
    check_horizontal_test "board setup has pieces b/t a2 and c2" "a2" "b2"
      board_setup false;
    check_horizontal_test "board setup has no pieces b/t d5 and h5" "d5" "h5"
      board_setup true;
    check_horizontal_test "kings pawn has pieces b/t a4 and h4" "a4" "h4"
      kings_pawn false;
    is_vertical_test "b4 and b8 are in the same column" "b4" "b8" true;
    is_vertical_test "b4 and g7 are not in the same column" "b4" "g7" false;
    check_vertical_test "board setup has no pieces b/t a6 and a3" "a6" "a3"
      board_setup true;
    check_vertical_test "board setup has pieces b/t g8 and g7" "g8" "g7"
      board_setup false;
    check_vertical_test "board setup has pieces b/t a8 and a5" "a8" "a5"
      board_setup false;
    check_vertical_test "board setup has pieces between e5 and e3" "e5" "e3"
      kings_pawn false;
    is_diagonal_test "c7 and f4 are in the same diagonal" "c7" "f4" true;
    is_diagonal_test "c6 and f4 are not in the same diagonal" "c6" "f4" false;
    is_diagonal_test "b5 and e8 are in the same diagonal" "b5" "e8" true;
    is_diagonal_test "b5 and f8 are not in the same diagonal" "b5" "f8" false;
    is_diagonal_test "g6 and b1 are in the same diagonal" "g6" "b1" true;
    is_diagonal_test "g6 and a1 are not in the same diagonal" "g6" "a1" false;
    is_diagonal_test "d5 and a8 are in the same diagonal" "d5" "a8" true;
    is_diagonal_test "d5 and b8 are not in the same diagonal" "d5" "b8" false;
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