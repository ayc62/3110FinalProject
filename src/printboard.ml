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

let piece state square =
  try
    let piece = List.assoc square state in
    piece_type_string piece
  with e -> "   "

let square_pos (row : int) (col : char) = Char.escaped col ^ string_of_int row

let rec print_row_helper state row col last_col next last_color =
  ANSITerminal.print_string
    [ (if last_color then ANSITerminal.on_white else ANSITerminal.on_yellow) ]
    (piece state (square_pos row col));
  if col = last_col then ANSITerminal.print_string [ ANSITerminal.default ] ""
  else
    print_row_helper state row
      (col |> Char.code |> next 1 |> Int.abs |> Char.chr)
      last_col next (not last_color)

let print_row state row col last_col next last_color =
  ANSITerminal.print_string [ ANSITerminal.default ]
    (" " ^ string_of_int row ^ " ");
  print_row_helper state row col last_col next last_color

let rec print_boarder_helper left_char right_char next =
  ANSITerminal.print_string [ ANSITerminal.default ]
    (" " ^ String.make 1 left_char ^ " ");
  if left_char = right_char then
    ANSITerminal.print_string [ ANSITerminal.default ] ""
  else
    print_boarder_helper
      (left_char |> Char.code |> next 1 |> Int.abs |> Char.chr)
      right_char next

let print_bottom_border left_char right_char next =
  ANSITerminal.print_string [ ANSITerminal.default ] "   ";
  print_boarder_helper left_char right_char next

let rec print_white state row bottom_row next_row col last_col next_col
    last_color =
  print_row state row col last_col next_col last_color;
  print_endline "";
  if row = bottom_row then (
    print_bottom_border col last_col next_col;
    print_endline "\n")
  else
    print_white state (next_row row 1) bottom_row next_row col last_col next_col
      (not last_color)

let print_board_helper row bottom_row next_row col last_col next_col last_color
    state =
  print_endline "";
  print_white state row bottom_row next_row col last_col next_col last_color

let print_captured color state =
  state |> get_captured_pieces
  |> List.filter (fun x -> x.piece_color = color)
  |> List.map piece_type_string
  |> List.fold_left (fun a b -> a ^ " " ^ b) ""

let print_header_message cur_variant rounds =
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("Current variant: "
    ^ string_of_variant cur_variant
    ^ " [" ^ string_of_rounds rounds ^ "] \n")

let print_board_white state cur_variant rounds =
  print_header_message cur_variant rounds;
  print_endline (state |> print_captured White);
  state |> get_board |> print_board_helper 8 1 ( - ) 'a' 'h' ( + ) false;
  print_endline ((state |> print_captured Black) ^ "\n")

let print_board_black state cur_variant rounds =
  print_header_message cur_variant rounds;
  print_endline (state |> print_captured Black);
  state |> get_board |> print_board_helper 1 8 ( + ) 'h' 'a' ( - ) true;
  print_endline ((state |> print_captured White) ^ "\n")
