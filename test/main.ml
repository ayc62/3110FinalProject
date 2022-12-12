open OUnit2
open Game
open Board
open Check
open Command
open Controller

let board_setup = Board.init_state
let board_tests = []

let kings_pawn =
  match move_piece Pawn White "e2" "e4" board_setup with
  | Legal st -> st
  | _ -> init_state

let is_horizontal_test (name : string) (orig_pos : string) (new_pos : string)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (is_horizontal orig_pos new_pos)

let check_horizontal_test (name : string) (orig_pos : string) (new_pos : string)
    (state : state) (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (check_horizontal orig_pos new_pos state)

let is_vertical_test (name : string) (orig_pos : string) (new_pos : string)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (is_vertical orig_pos new_pos)

let check_vertical_test (name : string) (orig_pos : string) (new_pos : string)
    (state : state) (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (check_vertical orig_pos new_pos state)

let is_diagonal_test (name : string) (orig_pos : string) (new_pos : string)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (is_diagonal orig_pos new_pos)

let check_diagonal_test (name : string) (orig_pos : string) (new_pos : string)
    (state : state) (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (check_diagonal orig_pos new_pos state)

let check_en_passant_test (name : string) (color : piece_color)
    (orig_pos : string) (new_pos : string) (state : state)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (check_en_passant color orig_pos new_pos state)

let check_castling_test (name : string) (color : piece_color)
    (orig_pos : string) (new_pos : string) (state : state)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (check_castle color orig_pos new_pos state)

let check_check_test (name : string) (color : piece_color) (state : state)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (check_check color state)

let possible_moves_test (name : string) (color : piece_color) (state : state)
    (expected_output : (Board.piece_type * string * string) list) =
  name >:: fun _ ->
  assert_equal expected_output (possible_moves color state state.board [])

let en_passant_state1 =
  {
    board =
      [
        piece_helper "c2" Pawn White;
        piece_helper "f5" Pawn White;
        piece_helper "d7" Pawn Black;
        piece_helper "e7" Pawn Black;
      ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let en_passant_state2 =
  {
    board =
      [
        piece_helper "c2" Pawn White;
        piece_helper "f5" Pawn White;
        piece_helper "d7" Pawn Black;
        piece_helper "e5" Pawn Black;
      ];
    old_boards = [ en_passant_state1.board ];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let en_passant_state3 =
  {
    board =
      [
        piece_helper "c4" Pawn White;
        piece_helper "f5" Pawn White;
        piece_helper "d7" Pawn Black;
        piece_helper "e5" Pawn Black;
      ];
    old_boards = [ en_passant_state2.board; en_passant_state1.board ];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let en_passant_state4 =
  {
    board =
      [
        piece_helper "c4" Pawn White;
        piece_helper "f5" Pawn White;
        piece_helper "d5" Pawn Black;
        piece_helper "e5" Pawn Black;
      ];
    old_boards =
      [
        en_passant_state3.board;
        en_passant_state2.board;
        en_passant_state1.board;
      ];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let en_passant_state5 =
  {
    board = [ piece_helper "f2" Pawn White; piece_helper "e3" Pawn Black ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let en_passant_state6 =
  {
    board = [ piece_helper "f2" Pawn White; piece_helper "e2" Pawn Black ];
    old_boards = [ en_passant_state5.board ];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let castle_state1 =
  {
    board = [ piece_helper "e1" King White; piece_helper "h1" Rook White ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let check_state1 =
  {
    board = [ piece_helper "e1" King White; piece_helper "b4" Bishop Black ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let check_state2 =
  {
    board = [ piece_helper "e1" King White; piece_helper "d2" Bishop Black ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let possible_moves_knight =
  {
    board = [ piece_helper "e4" Knight White ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let check_tests =
  [
    (* is_horizontal_test "a3 and g3 are in the same row" "a3" "g3" true;
       is_horizontal_test "a3 and c5 are not in the same row" "a3" "c5" false;
       check_horizontal_test "board setup has no pieces b/t a3 and c3" "a3" "b3"
       board_setup true; check_horizontal_test "board setup has pieces b/t a2
       and c2" "a2" "c2" board_setup false; check_horizontal_test "board setup
       has no pieces b/t d5 and h5" "d5" "h5" board_setup true;
       check_horizontal_test "kings pawn has pieces b/t a4 and h4" "a4" "h4"
       kings_pawn false; is_vertical_test "b4 and b8 are in the same column"
       "b4" "b8" true; is_vertical_test "b4 and g7 are not in the same column"
       "b4" "g7" false; check_vertical_test "board setup has no pieces b/t a6
       and a3" "a6" "a3" board_setup true; check_vertical_test "board setup has
       no pieces b/t g8 and g7" "g8" "g7" board_setup true; check_vertical_test
       "board setup has pieces b/t a8 and a5" "a8" "a5" board_setup false;
       check_vertical_test "board setup has pieces between e5 and e3" "e5" "e3"
       kings_pawn false; is_diagonal_test "c7 and f4 are in the same diagonal"
       "c7" "f4" true; is_diagonal_test "c6 and f4 are not in the same diagonal"
       "c6" "f4" false; is_diagonal_test "b5 and e8 are in the same diagonal"
       "b5" "e8" true; is_diagonal_test "b5 and f8 are not in the same diagonal"
       "b5" "f8" false; is_diagonal_test "g6 and b1 are in the same diagonal"
       "g6" "b1" true; is_diagonal_test "g6 and a1 are not in the same diagonal"
       "g6" "a1" false; is_diagonal_test "d5 and a8 are in the same diagonal"
       "d5" "a8" true; is_diagonal_test "d5 and b8 are not in the same diagonal"
       "d5" "b8" false; check_diagonal_test "board setup has no pieces between
       c3 and a5" "c3" "a5" board_setup true; check_diagonal_test "board setup
       has pieces between c3 and h8" "c3" "h8" board_setup false;
       check_diagonal_test "board setup has pieces between c3 and e1" "c3" "e1"
       board_setup false; check_diagonal_test "board setup has pieces between c3
       and a1" "c3" "a1" board_setup false; *)
    check_en_passant_test "valid en passant" White "f5" "e6" en_passant_state2
      true;
    check_en_passant_test "invalid en passant" White "f5" "e6" en_passant_state4
      false;
    check_en_passant_test "invalid en passant 2" White "f2" "e3"
      en_passant_state6 false;
    (* check_castling_test "valid castle" White "e1" "g1" castle_state1 true; *)
    check_check_test "black check" Black check_state1 true;
    check_check_test "black and white check" Black check_state2 true;
    possible_moves_test "knight moves" White possible_moves_knight
      [
        (Knight, "e4", "c5");
        (Knight, "e4", "c3");
        (Knight, "e4", "d6");
        (Knight, "e4", "d2");
        (Knight, "e4", "f2");
        (Knight, "e4", "f6");
        (Knight, "e4", "g3");
        (Knight, "e4", "g5");
      ];
  ]

let command_tests = []
let controller_tests = []

let suite =
  "chess project test suite"
  >::: List.flatten
         [ board_tests; check_tests; command_tests; controller_tests ]

let _ = run_test_tt_main suite