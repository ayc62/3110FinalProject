open Board

let piece_type_string piece =
  match piece.piece_type with
  | Rook -> "R"
  | Knight -> "N"
  | Bishop -> "B"
  | Queen -> "Q"
  | King -> "K"
  | Pawn -> "P"

let piece_color_string piece =
  match piece.piece_color with
  | White -> "w"
  | Black -> "b"

let piece state square =
  try
    let piece = List.assoc square state in
    piece_color_string piece ^ piece_type_string piece
  with e -> "  "

let square_pos (row : int) (col : char) = Char.escaped col ^ string_of_int row

let rec print_row_helper state row col last_col next =
  "| "
  ^ piece state (square_pos row col)
  ^ " "
  ^
  if col = last_col then "|"
  else
    print_row_helper state row
      (col |> Char.code |> next 1 |> Int.abs |> Char.chr)
      last_col next

let print_row state row col last_col next =
  "| " ^ string_of_int row ^ " " ^ print_row_helper state row col last_col next

let rec print_boarder_helper left_char right_char next =
  " |  " ^ String.make 1 left_char
  ^
  if left_char = right_char then " |"
  else
    print_boarder_helper
      (left_char |> Char.code |> next 1 |> Int.abs |> Char.chr)
      right_char next

let print_bottom_border left_char right_char next =
  "   " ^ print_boarder_helper left_char right_char next

let rec print_white state row bottom_row next_row col last_col next_col =
  print_endline "     ---------------------------------------";
  print_endline (print_row state row col last_col next_col);
  if row = bottom_row then (
    print_endline "     ---------------------------------------";
    print_endline (print_bottom_border col last_col next_col))
  else
    print_white state (next_row row 1) bottom_row next_row col last_col next_col

let print_board_helper row bottom_row next_row col last_col next_col state =
  print_endline "";
  print_white state row bottom_row next_row col last_col next_col

let print_board_white state =
  state |> get_board |> print_board_helper 8 1 ( - ) 'a' 'h' ( + )

let print_board_black state =
  state |> get_board |> print_board_helper 1 8 ( + ) 'h' 'a' ( - )
