open OUnit2
open Game
open Board
open Check
open Command
open Controller

(* *)

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let to_str piece =
  match piece with
  | Pawn -> "Pawn"
  | Knight -> "Knight"
  | Rook -> "Rook"
  | Queen -> "Queen"
  | Bishop -> "Bishop"
  | King -> "King"

let rec print_lst = function
  | [] -> print_endline "-----------------"
  | (piece, orig_pos, new_pos) :: t ->
      print_endline (to_str piece ^ " " ^ orig_pos ^ " -> " ^ new_pos);
      print_lst t

let board_setup = Board.init_state

let kings_pawn =
  match move_piece Pawn White "e2" "e4" board_setup with
  | Legal st -> st
  | _ -> init_state

let is_square_test (name : string) (pos : string) (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (check_square pos)

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

let check_piece_move_test (name : string) (piece : piece_type)
    (color : piece_color) (orig_pos : string) (new_pos : string) (state : state)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (check_piece_move piece color orig_pos new_pos state)

let possible_moves_test (name : string) (color : piece_color) (state : state)
    (expected_output : (Board.piece_type * string * string) list) =
  name >:: fun _ ->
  assert (
    cmp_set_like_lists expected_output
      (possible_moves color state state.board []))

let parse_move_test (name : string) (command : string list) (expected : command)
    (command_excn : bool) (square_excn : bool) =
  name >:: fun _ ->
  try
    let com = parse_move command in
    assert_equal com expected
  with
  | InvalidCommand -> assert command_excn
  | InvalidSquare -> assert square_excn

let parse_test (name : string) (str : string) (color : piece_color)
    (state : state) (expected : command) (excn : bool) =
  name >:: fun _ ->
  try
    let com = parse str color state in
    assert_equal com expected
  with InvalidCommand -> assert excn

let parse_promotion_test (name : string) (str : string) (expected : piece_type)
    (excn : bool) =
  name >:: fun _ ->
  try
    let promo = parse_promotion str in
    assert_equal promo expected
  with InvalidPiece -> assert excn

let parse_variant_test (name : string) (str : string) (var : variant)
    (excn : bool) =
  name >:: fun _ ->
  try
    let out = parse_variant str in
    assert_equal out var
  with InvalidVariant -> assert excn

let parse_rounds_test (name : string) (str : string) (rounds : rounds)
    (excn : bool) =
  name >:: fun _ ->
  try
    let out = parse_rounds str in
    assert_equal out rounds
  with InvalidVariant -> assert excn

let parse_response_test (name : string) (str : string) (resp : response)
    (excn : bool) =
  name >:: fun _ ->
  try
    let out = parse_response str in
    assert_equal out resp
  with InvalidResponse -> assert excn

let string_of_variant_test (name : string) (v : variant)
    (expected_output : string) (excn : bool) =
  name >:: fun _ ->
  try
    let out = string_of_variant v in
    assert_equal out expected_output
  with InvalidResponse -> assert excn

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

let kingside_castle =
  {
    board = [ piece_helper "e1" King White; piece_helper "h1" Rook White ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let queenside_castle =
  {
    board = [ piece_helper "e1" King White; piece_helper "a1" Rook White ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let invalid_castle1 =
  {
    board =
      [
        piece_helper "e1" King White;
        piece_helper "h1" Rook White;
        piece_helper "f1" Queen White;
      ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let invalid_castle2 =
  {
    board =
      [
        piece_helper "e1" King White;
        piece_helper "h1" Rook White;
        piece_helper "b4" Bishop Black;
      ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let invalid_castle3 =
  {
    board =
      [
        piece_helper "e1" King White;
        piece_helper "h1" Rook White;
        piece_helper "a6" Bishop Black;
      ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let invalid_castle4 =
  {
    board =
      [
        piece_helper "e1" King White;
        piece_helper "h1" Rook White;
        piece_helper "c5" Bishop Black;
      ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let invalid_castle5 =
  {
    board =
      [
        piece_helper ~moved:true "e1" King White;
        piece_helper ~moved:true "h1" Rook White;
      ];
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

let possible_moves piece =
  {
    board = [ piece_helper "e4" piece White ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let possible_moves_pawn_moved =
  {
    board = [ piece_helper ~moved:true "e6" Pawn White ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let possible_moves_king_restricted =
  {
    board =
      [
        piece_helper "f3" King White;
        piece_helper "d6" Bishop Black;
        piece_helper "g1" Bishop Black;
      ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let possible_attack piece pos1 pos2 =
  {
    board = [ piece_helper pos1 piece White; piece_helper pos2 Pawn Black ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let check_tests =
  [
    is_square_test "not a square" "a" false;
    is_square_test "not a square 2" "a9" false;
    is_horizontal_test "a3 and g3 are in the same row" "a3" "g3" true;
    is_horizontal_test "a3 and c5 are not in the same row" "a3" "c5" false;
    check_horizontal_test "board setup has no pieces b/t a3 and c3" "a3" "b3"
      board_setup true;
    check_horizontal_test "board setup has pieces b/t a2 and c2" "a2" "c2"
      board_setup false;
    check_horizontal_test "board setup has no pieces b/t d5 and h5" "d5" "h5"
      board_setup true;
    check_horizontal_test "kings pawn has pieces b/t a4 and h4" "a4" "h4"
      kings_pawn false;
    is_vertical_test "b4 and b8 are in the same column" "b4" "b8" true;
    is_vertical_test "b4 and g7 are not in the same column" "b4" "g7" false;
    check_vertical_test "board setup has no pieces b/t a6 and a3" "a6" "a3"
      board_setup true;
    check_vertical_test "board setup has no pieces b/t g8 and g7" "g8" "g7"
      board_setup true;
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
    check_en_passant_test "valid en passant" White "f5" "e6" en_passant_state2
      true;
    check_en_passant_test "invalid en passant" White "f5" "e6" en_passant_state4
      false;
    check_en_passant_test "invalid en passant 2" White "f2" "e3"
      en_passant_state6 false;
    check_castling_test "kingside castle" White "e1" "g1" kingside_castle true;
    check_castling_test "queenside castle" White "e1" "c1" queenside_castle true;
    check_castling_test "castling with piece in between" White "e1" "g1"
      invalid_castle1 false;
    check_castling_test "castling when check" White "e1" "g1" invalid_castle2
      false;
    check_castling_test "castling through check" White "e1" "g1" invalid_castle3
      false;
    check_castling_test "castling into check" White "e1" "g1" invalid_castle4
      false;
    check_castling_test "castling after moved" White "e1" "g1" invalid_castle5
      false;
    check_check_test "black check" Black check_state1 true;
    check_check_test "black and white check" Black check_state2 true;
    check_piece_move_test "pawn valid move" Pawn White "e4" "e6"
      (possible_moves Pawn) true;
    check_piece_move_test "pawn invalid move" Pawn White "e4" "e6"
      possible_moves_pawn_moved false;
    check_piece_move_test "knight invalid move" Knight White "e4" "a7"
      (possible_moves Knight) false;
    check_piece_move_test "rook invalid move" Rook White "e4" "a7"
      (possible_moves Rook) false;
    check_piece_move_test "bishop invalid move" Bishop White "e4" "a7"
      (possible_moves Bishop) false;
    check_piece_move_test "queen invalid move" Queen White "e4" "a7"
      (possible_moves Queen) false;
    check_piece_move_test "king invalid move" King White "e4" "a7"
      (possible_moves King) false;
    check_piece_move_test "pawn valid attack" Pawn White "e4" "f5"
      (possible_attack Pawn "e4" "f5")
      true;
    check_piece_move_test "king valid attack" King White "e4" "f5"
      (possible_attack King "e4" "f5")
      true;
    check_piece_move_test "bishop valid attack" Bishop White "e4" "f5"
      (possible_attack Bishop "e4" "f5")
      true;
    check_piece_move_test "queen valid attack" Queen White "e4" "f5"
      (possible_attack Queen "e4" "f5")
      true;
    check_piece_move_test "rook valid attack" Rook White "e5" "f5"
      (possible_attack Rook "e5" "f5")
      true;
    check_piece_move_test "rook invalid attack" Rook White "e5" "a7"
      (possible_attack Rook "e5" "a7")
      false;
    possible_moves_test "knight moves" White (possible_moves Knight)
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
    possible_moves_test "unmoved pawn moves" White (possible_moves Pawn)
      [ (Pawn, "e4", "e5"); (Pawn, "e4", "e6") ];
    possible_moves_test "moved pawn moves" White possible_moves_pawn_moved
      [ (Pawn, "e6", "e7") ];
    possible_moves_test "king moves" White (possible_moves King)
      [
        (King, "e4", "d3");
        (King, "e4", "e3");
        (King, "e4", "f3");
        (King, "e4", "d4");
        (King, "e4", "f4");
        (King, "e4", "d5");
        (King, "e4", "e5");
        (King, "e4", "f5");
      ];
    possible_moves_test "king moves restricted" White
      possible_moves_king_restricted
      [
        (King, "f3", "e2");
        (King, "f3", "g2");
        (King, "f3", "e4");
        (King, "f3", "g4");
      ];
    possible_moves_test "bishop moves" White (possible_moves Bishop)
      [
        (Bishop, "e4", "d3");
        (Bishop, "e4", "c2");
        (Bishop, "e4", "b1");
        (Bishop, "e4", "f5");
        (Bishop, "e4", "g6");
        (Bishop, "e4", "h7");
        (Bishop, "e4", "f3");
        (Bishop, "e4", "g2");
        (Bishop, "e4", "h1");
        (Bishop, "e4", "d5");
        (Bishop, "e4", "c6");
        (Bishop, "e4", "b7");
        (Bishop, "e4", "a8");
      ];
    possible_moves_test "rook moves" White (possible_moves Rook)
      [
        (Rook, "e4", "e3");
        (Rook, "e4", "e2");
        (Rook, "e4", "e1");
        (Rook, "e4", "e5");
        (Rook, "e4", "e6");
        (Rook, "e4", "e7");
        (Rook, "e4", "e8");
        (Rook, "e4", "a4");
        (Rook, "e4", "b4");
        (Rook, "e4", "c4");
        (Rook, "e4", "d4");
        (Rook, "e4", "f4");
        (Rook, "e4", "g4");
        (Rook, "e4", "h4");
      ];
    possible_moves_test "queen moves" White (possible_moves Queen)
      [
        (Queen, "e4", "a8");
        (Queen, "e4", "b7");
        (Queen, "e4", "c6");
        (Queen, "e4", "d5");
        (Queen, "e4", "f3");
        (Queen, "e4", "g2");
        (Queen, "e4", "h1");
        (Queen, "e4", "b1");
        (Queen, "e4", "c2");
        (Queen, "e4", "d3");
        (Queen, "e4", "f5");
        (Queen, "e4", "g6");
        (Queen, "e4", "h7");
        (Queen, "e4", "e1");
        (Queen, "e4", "e2");
        (Queen, "e4", "e3");
        (Queen, "e4", "e5");
        (Queen, "e4", "e6");
        (Queen, "e4", "e7");
        (Queen, "e4", "e8");
        (Queen, "e4", "a4");
        (Queen, "e4", "b4");
        (Queen, "e4", "c4");
        (Queen, "e4", "d4");
        (Queen, "e4", "f4");
        (Queen, "e4", "g4");
        (Queen, "e4", "h4");
      ];
  ]

let command_tests =
  [
    parse_move_test "Empty list input" [] Resign true false;
    parse_move_test "Two element list input" [ "pawn"; "e1" ] Resign true false;
    parse_move_test "Correct list input" [ "pawn"; "e1"; "e2" ]
      (Move (Pawn, [ "e1"; "e2" ]))
      false false;
    parse_move_test "Incorrect square input" [ "pawn"; "x1"; "e2" ]
      (Move (Pawn, [ "e1"; "e2" ]))
      false true;
    parse_test "Castle kingside" "castle kingside" White kingside_castle
      (Move (King, [ "e1"; "g1" ]))
      false;
    parse_test "Castle queenside" "castle queenside" White queenside_castle
      (Move (King, [ "e1"; "c1" ]))
      false;
    parse_test "Castle queenside spaces" "  castle    queenside  " White
      queenside_castle
      (Move (King, [ "e1"; "c1" ]))
      false;
    parse_test "Empty command" "" White queenside_castle
      (Move (King, [ "e1"; "c1" ]))
      true;
    parse_test "Move command" "move pawn e1 e2" White init_state
      (Move (Pawn, [ "e1"; "e2" ]))
      false;
    parse_test "Draw command" "draw asdf asdf" White init_state DrawOffer false;
    parse_test "Resign command" "resign asdf asdf" White init_state Resign false;
    parse_test "Invalid command" "asdf asdf" White init_state Resign true;
    parse_promotion_test "Promote to Queen" "Queen" Queen false;
    parse_promotion_test "Promote to Pawn" "Pawn" Pawn true;
    parse_promotion_test "Promote to Queen with spaces" "  Queen " Queen false;
    parse_promotion_test "Promote to asdf" "asdf" Queen true;
    parse_variant_test "Fischer" "Fischer    Random" FischerRandom false;
    parse_variant_test "Fischer lowercase" "fischer random" FischerRandom true;
    parse_variant_test "Standard" " Standard " Standard false;
    parse_variant_test "KOTH" " KOTH " KingOfTheHill false;
    parse_rounds_test "Single" "single" (BestOf 1) false;
    parse_rounds_test "Single case" "SiNgLe" (BestOf 1) false;
    parse_rounds_test "Best of non-int" "bet of d" (BestOf 1) true;
    parse_rounds_test "Best of 3" "best of 3" (BestOf 3) false;
    parse_response_test "Empty response" "" No true;
    parse_response_test "Many word response" "a b c d " No true;
    parse_response_test "yes response" "y" Yes false;
    parse_response_test "no response" "n" No false;
    parse_response_test "no response case" "N" No false;
    string_of_variant_test "standard string" Standard "Standard" false;
    string_of_variant_test "3-check string" ThreeCheck "3-check" false;
    string_of_variant_test "KOTH string" KingOfTheHill "King of the Hill" false;
    string_of_variant_test "fischer random string" FischerRandom
      "Fischer Random" false;
  ]

let controller_tests = []

let suite =
  "chess project test suite"
  >::: List.flatten [ check_tests; command_tests; controller_tests ]

let _ = run_test_tt_main suite
