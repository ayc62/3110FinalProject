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
   chessboard [old_state] and a value of type result, which indicates the
   resulting situation of the chessboard as a result of the move.*)

val promote_pawn :
  Board.piece_color -> string -> Board.piece_type -> Board.state -> result
(**[promote_pawn] promotes a pawn to a specified piece type of the player's
   choice. *)
