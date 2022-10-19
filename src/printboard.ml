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

let rec print_row state row col =
  "| "
  ^ piece state (square_pos row col)
  ^ " "
  ^
  if col = 'h' then "|"
  else print_row state row (col |> Char.code |> ( + ) 1 |> Int.abs |> Char.chr)

let rec print state row =
  print_endline "-----------------------------------------";
  print_endline (print_row state row 'a');
  if row = 1 then print_endline "-----------------------------------------"
  else print state (row - 1)

let print_board_helper state =
  print_endline "";
  print state 8

let print_board state = state |> get_board |> print_board_helper