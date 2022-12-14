(** Interactions between the board and the chess logic *)

(** type [result] represents the possible outcomes of any given chess move. *)
type result =
  | Legal of Board.state
  | Illegal
  | Check of Board.state
  | Draw of Board.state
  | Checkmate of Board.state
  | Stalemate of Board.state
  | PawnPromotion of Board.state

val move_piece :
  Board.piece_type ->
  Board.piece_color ->
  string ->
  string ->
  Board.state ->
  result
(**[move_piece] moves a piece [piece] of color [color] from an old position
   [orig_pos] to a new position [new_pos]. It also takes in the old state of the
   chessboard [old_state] and a value of type result, which indicates the
   resulting situation of the chessboard as a result of the move.*)

val update_board_state : Board.piece_type -> Board.piece_color -> string -> string -> Board.state 
-> Board.board -> Board.state
(** [update_board_state piece color orig_pos new_pos state board] removes piece
    [piece] with color [color] that moves from position [orig_pos] to position
    [new_pos] in the state [state]. [board] is the board that will be changed
    and returned*)

val promote_pawn :
  Board.piece_color -> string -> Board.piece_type -> Board.state -> result
(**[promote_pawn] promotes a pawn to a specified piece type of the player's
   choice. *)
