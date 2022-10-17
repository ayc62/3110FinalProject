(* Note: You may introduce new code anywhere in this file. *)
open Board

type result =
  | Legal of state
  | Illegal

let move_piece (piece : piece_type) (orig_pos : string) (new_pos : string)
    (old_state : state) : state =
  old_state
