(* Note: You may introduce new code anywhere in this file. *)
open Board

type result =
  | Legal of state
  | Illegal

(**[move_piece] moves a piece [piece] of color [color] from an old position
   [orig_pos] to a new position [new_pos]. It also takes in the old state of the
   chessboard [old_state] and returns the new state of the chessboard with the
   move applied. The state of the chessboard is represented as an associated
   list of positions as keys and a record with the piece type and piece color as
   the entry.

   NOTE: So far, we have not implemented if checking functions to check if a
   move is valid or not. So right now, any move to any square is valid.*)
let move_piece (piece : piece_type) (color : piece_color) (orig_pos : string)
    (new_pos : string) (old_state : state) (old_states : state list) : state =
  piece_helper ~moved:true new_pos piece color
  :: List.remove_assoc orig_pos old_state
