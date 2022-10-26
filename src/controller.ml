(* Note: You may introduce new code anywhere in this file. *)
open Board
open Check

type result =
  | Legal of state
  | Illegal
  | Check of state
  | Draw of state
  | CheckMate of state

let castle color orig_pos new_pos state board =
  let dir = diff new_pos orig_pos 0 / abs (diff new_pos orig_pos 0) in
  let col =
    String.get orig_pos 0 |> Char.code |> ( + ) dir |> Char.chr |> String.make 1
  in
  let row = String.get orig_pos 1 |> String.make 1 in
  let rook_pos = castle_rook col row dir state in
  match rook_pos with
  | None -> board
  | Some pos ->
      piece_helper ~moved:true (col ^ row) Rook color
      :: (board |> List.remove_assoc pos)

let remove_piece piece color orig_pos new_pos (state : state) board =
  match board |> List.assoc_opt new_pos with
  | None ->
      if piece = Pawn && check_en_passant color orig_pos new_pos state then
        let dir = if color = White then 1 else -1 in
        board |> List.remove_assoc (new_pos |> move_vertical (-1 * dir))
      else if piece = King && check_castle color orig_pos new_pos state then
        castle color orig_pos new_pos state board
      else board
  | Some piece -> board |> List.remove_assoc new_pos

(** [move_piece_helper piece color orig_pos new_pos cur_state] is the state
    where the piece has moved from position [old_pos] to position [new_pos]*)
let move_piece_helper piece color orig_pos new_pos cur_state =
  {
    cur_state with
    board =
      piece_helper ~moved:true new_pos piece color
      :: (cur_state |> get_board |> List.remove_assoc orig_pos
         |> remove_piece piece color orig_pos new_pos cur_state);
  }

(**[check_checkmate color state] checks if color [color] managed to checkmate
   the opponent*)
let check_checkmate color state = false

(**[move_piece] moves a piece [piece] of color [color] from an old position
   [orig_pos] to a new position [new_pos]. It also takes in the old state of the
   chessboard [old_state] and returns the new state of the chessboard with the
   move applied. The state of the chessboard is represented as an associated
   list of positions as keys and a record with the piece type and piece color as
   the entry.*)
let move_piece (piece : piece_type) (color : piece_color) (orig_pos : string)
    (new_pos : string) (cur_state : state) =
  if not (check_piece_move piece color orig_pos new_pos cur_state) then Illegal
  else
    let new_state = move_piece_helper piece color orig_pos new_pos cur_state in
    if check_check (opp_color color) new_state then Illegal
    else
      Legal
        {
          (move_piece_helper piece color orig_pos new_pos cur_state) with
          old_boards = get_board cur_state :: get_old_boards cur_state;
        }
