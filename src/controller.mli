(** Interactions between the board and the chess logic *)

(** The possible outcomes of any given chess move *)
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
   chessboard [old_state] and returns the new state of the chessboard with the
   move applied. The state of the chessboard is represented as an associated
   list of positions as keys and a record with the piece type and piece color as
   the entry.*)