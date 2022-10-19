(* Note: You may introduce new code anywhere in this file. *)
open Board
open Check

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
    (new_pos : string) (cur_state : state) =
  if not (check_valid_move piece color orig_pos new_pos cur_state) then Illegal
  else
    Legal
      {
        cur_state with
        board =
          piece_helper ~moved:true new_pos piece color
          :: List.remove_assoc orig_pos (get_board cur_state);
        old_boards = get_board cur_state :: get_old_boards cur_state;
      }
