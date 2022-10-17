(* Note: You may introduce new code anywhere in this file. *)
open Board

type result =
  | Legal of state
  | Illegal

let move_piece (piece : piece_type) (color : piece_color) (orig_pos : string)
    (new_pos : string) (old_state : state) : state =
  piece_helper new_pos piece color :: List.remove_assoc orig_pos old_state
