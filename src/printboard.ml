open Board
open Command

let piece_type_string piece =
  match piece.piece_color with
  | White -> begin
      match piece.piece_type with
      | Rook -> " ♖ "
      | Knight -> " ♘ "
      | Bishop -> " ♗ "
      | Queen -> " ♕ "
      | King -> " ♔ "
      | Pawn -> " ♙ "
    end
  | Black -> begin
      match piece.piece_type with
      | Rook -> " ♜ "
      | Knight -> " ♞ "
      | Bishop -> " ♝ "
      | Queen -> " ♛ "
      | King -> " ♚ "
      | Pawn -> " ♟︎ "
    end

let piece_color_string piece =
  match piece.piece_color with
  | White -> "w"
  | Black -> "b"

let piece state square =
  try
    let piece = List.assoc square state in
    piece_type_string piece
  with e -> "   "

let square_pos (row : int) (col : char) = Char.escaped col ^ string_of_int row

let rec print_row_helper state row col last_col next last_color =
  ANSITerminal.print_string [if last_color then ANSITerminal.on_white else ANSITerminal.on_yellow] 
  (piece state (square_pos row col));
  if col = last_col then ANSITerminal.print_string [ANSITerminal.default] ""
  else
    print_row_helper state row
      (col |> Char.code |> next 1 |> Int.abs |> Char.chr)
      last_col next (not last_color)

let print_row state row col last_col next last_color =
  ANSITerminal.print_string [ANSITerminal.default] (" " ^ string_of_int row ^ " ");
  print_row_helper state row col last_col next last_color

let rec print_boarder_helper left_char right_char next =
  ANSITerminal.print_string [ANSITerminal.default] (" " ^ String.make 1 left_char ^ " ");
  if left_char = right_char then (ANSITerminal.print_string [ANSITerminal.default] "") else
    print_boarder_helper
      (left_char |> Char.code |> next 1 |> Int.abs |> Char.chr)
      right_char next

let print_bottom_border left_char right_char next =
  ANSITerminal.print_string [ANSITerminal.default] "   ";
  print_boarder_helper left_char right_char next

let rec print_white state row bottom_row next_row col last_col next_col last_color=
  print_row state row col last_col next_col last_color;
  print_endline "";
  if row = bottom_row then (
    print_bottom_border col last_col next_col;
    print_endline "";)
  else
    print_white state (next_row row 1) bottom_row next_row col last_col next_col (not last_color)

let print_board_helper row bottom_row next_row col last_col next_col state =
  print_endline "";
  print_white state row bottom_row next_row col last_col next_col

let print_board_white state =
  state |> get_board |> print_board_helper 8 1 ( - ) 'a' 'h' ( + )

let print_board_black state =
  state |> get_board |> print_board_helper 1 8 ( + ) 'h' 'a' ( - )
