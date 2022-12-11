type piece_type =
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  | Pawn  (**The abstract type representing what type a piece is*)

type piece_color =
  | White
  | Black  (**The abstract type representing the color of a piece*)

type piece_state = {
  piece_type : piece_type;
  piece_color : piece_color;
  moved : bool;
}
(**The abstract type representing a piece*)

val piece_helper :
  ?moved:bool -> string -> piece_type -> piece_color -> string * piece_state
(**[piece_helper] takes in the position [pos] of a piece, and the type [p_type]
   and color [p_color] of the piece, and returns it in an association list entry
   format. [pos] is the key, and the value is a record with [p_type] and
   [p_color].*)

val get_piece_type : piece_state -> piece_type
(**[get_piece_type] returns the type of the piece*)

val get_piece_color : piece_state -> piece_color
(**[get_piece_color] returns the color of the piece*)

val get_moved : piece_state -> bool
(**[get_moved] returns whether or not the piece has been moved*)

type board = (string * piece_state) list
(**The abstract type representing the chessboard*)

type state = {
  board : (string * piece_state) list;
  old_boards : board list;
  fifty_move_rule : int;
  captured_pieces : piece_state list;
  num_repetition : int;
}
(**The abstract type representing the current and past states of the chessboard*)

val get_board : state -> (string * piece_state) list
(**[get_board] returns the chessboard*)

val get_old_boards : state -> board list
(**[get_old_boards] returns the chessboards in previous moves*)

val get_fifty_move_rule : state -> int
(**[get_fifty_move_rule] returns the number of moves since last capture*)

val get_captured_pieces : state -> piece_state list
(**[get_fifty_move_rule] returns the number of moves since last capture*)

val get_num_repetitions : state -> int
(**[get_fifty_move_rule] returns the number of moves since last capture*)

val init_state : state
(**[init_state] is the initial state of the chessboard*)

val fischer_random_state : state

val square_has_pt : state -> string -> piece_type -> piece_color -> bool
(** [square_has_pt state square piece_type piece_color] returns whether a piece
    of type piece_type and color piece_color is found in the square sq of the
    board of state. If no piece corresponds to that square, this returns false.*)
