(** Implementation of a chessboard in OCaml *)

(**[piece_type] represents what type of piece something is.*)
type piece_type =
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  | Pawn

(**[piece_color] represents whether a piece is black or white.*)
type piece_color =
  | White
  | Black

type piece_state = {
  piece_type : piece_type;
  piece_color : piece_color;
  moved : bool;
}
(**[piece_state] is the abstract type representing a piece.*)

val piece_helper :
  ?moved:bool -> string -> piece_type -> piece_color -> string * piece_state
(**[piece_helper] takes in the position [pos] of a piece, and the type [p_type]
   and color [p_color] of the piece, and returns it in an association list entry
   format. [pos] is the key, and the value is a record with [p_type] and
   [p_color].*)

val get_piece_type : piece_state -> piece_type
(**[get_piece_type] returns the type of the piece.*)

val get_piece_color : piece_state -> piece_color
(**[get_piece_color] returns the color of the piece.*)

val get_moved : piece_state -> bool
(**[get_moved] returns whether or not the piece has been moved.*)

type board = (string * piece_state) list
(**[board] represents a chessboard at a given time.*)

type state = {
  board : (string * piece_state) list;
  old_boards : board list;
  fifty_move_rule : int;
  captured_pieces : piece_state list;
  num_repetition : int;
}
(**[state] represents a chessboard, along with its past forms and various other
   markers that determine whether a draw has taken place. *)

val get_board : state -> (string * piece_state) list
(**[get_board] returns the chessboard/*)

val get_old_boards : state -> board list
(**[get_old_boards] returns the chessboards in previous moves.*)

val get_fifty_move_rule : state -> int
(**[get_fifty_move_rule] returns the number of moves since last capture.*)

val get_captured_pieces : state -> piece_state list
(**[get_fifty_move_rule] returns the number of moves since last capture.*)

val get_num_repetitions : state -> int
(**[get_fifty_move_rule] returns the number of moves since last capture.*)

val init_state : state
(**[init_state] is the initial state of the chessboard.*)

val fischer_random_state : unit -> state
(**[fischer_random_state] generates a random initial state of the chessboard in
   accordance to the rules for Fischer Random chess.*)

val square_has_pt : state -> string -> piece_type -> piece_color -> bool
(** [square_has_pt state square piece_type piece_color] returns whether a piece
    of type piece_type and color piece_color is found in the square sq of the
    board of state. If no piece corresponds to that square, this returns false.*)
