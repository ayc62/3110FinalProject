(* Note: You may introduce new code anywhere in this file. *)
open Board
open Check

type result =
  | Legal of state
  | Illegal
  | Check of state
  | Draw of state
  | Checkmate of state
  | Stalemate of state
  | PawnPromotion of state

(** [castle color orig_pos new_pos state board] castles [color] in the current*)
let castle color orig_pos new_pos state board =
  let dir = if String.get new_pos 0 = 'g' then 1 else -1 in
  let new_rook_pos = move_horizontal (-1 * dir) new_pos in
  let rook_pos = castle_rook (move_horizontal dir orig_pos) dir state in
  match rook_pos with
  | None -> failwith "Why is there no rook??"
  | Some pos -> (
      match List.assoc_opt new_rook_pos board with
      | None ->
          piece_helper ~moved:true new_rook_pos Rook color
          :: (board |> List.remove_assoc pos)
      | Some piece ->
          (new_rook_pos, piece)
          :: piece_helper ~moved:true new_rook_pos Rook color
          :: (board |> List.remove_assoc pos |> List.remove_assoc new_rook_pos))

(** [update_board_state piece color orig_pos new_pos state board] removes piece
    [piece] with color [color] that moves from position [orig_pos] to position
    [new_pos] in the state [state]. [board] is the board that will be changed
    and returned*)
let update_board_state piece color orig_pos new_pos (state : state) board =
  let updated_state =
    match board |> List.assoc_opt new_pos with
    | None ->
        if piece = Pawn then
          if check_en_passant color orig_pos new_pos state then
            let dir = if color = White then 1 else -1 in
            {
              state with
              board =
                board |> List.remove_assoc (new_pos |> move_vertical (-1 * dir));
              fifty_move_rule = 0;
              captured_pieces =
                (board |> List.assoc (new_pos |> move_vertical (-1 * dir)))
                :: (state |> get_captured_pieces);
            }
          else { state with board; fifty_move_rule = 0 }
        else if piece = King && check_castle color orig_pos new_pos state then
          {
            state with
            board = castle color orig_pos new_pos state board;
            fifty_move_rule = (state |> get_fifty_move_rule) + 1;
          }
        else
          {
            state with
            board;
            fifty_move_rule = (state |> get_fifty_move_rule) + 1;
          }
    | Some captured_piece ->
        if piece = King && check_castle color orig_pos new_pos state then
          {
            state with
            board = castle color orig_pos new_pos state board;
            fifty_move_rule = (state |> get_fifty_move_rule) + 1;
          }
        else
          {
            state with
            board = board |> List.remove_assoc new_pos;
            captured_pieces =
              (board |> List.assoc new_pos) :: (state |> get_captured_pieces);
            fifty_move_rule = 0;
          }
  in
  {
    updated_state with
    board =
      piece_helper ~moved:true new_pos piece color
      :: (updated_state |> get_board |> List.remove_assoc orig_pos);
  }

(*[equal_board_state board1 board2] checks to see if board1 is equal to board2*)
let rec equal_board_state board1 board2 =
  match board2 with
  | [] -> true
  | h :: t -> (
      let pos = fst h in
      let piece_state = snd h in
      match List.assoc_opt pos board1 with
      | None -> false
      | Some s ->
          if
            s.piece_type = piece_state.piece_type
            && s.piece_color = piece_state.piece_color
          then equal_board_state board1 t
          else false)

let rec update_num_repetitions acc old_boards state =
  match old_boards with
  | [] -> { state with num_repetition = acc }
  | h :: t ->
      if h |> List.length <> (state |> get_board |> List.length) then
        { state with num_repetition = acc }
      else if equal_board_state h (state |> get_board) then
        update_num_repetitions (acc + 1) t state
      else update_num_repetitions acc t state

(** [move_piece_helper piece color orig_pos new_pos cur_state] is the state
    where the piece has moved from position [old_pos] to position [new_pos]*)
let move_piece_helper piece color orig_pos new_pos cur_state =
  cur_state |> get_board
  |> update_board_state piece color orig_pos new_pos cur_state

(** [all_valid_moves_helper color moves state acc] is all the valid moves
    [color] can actually make from the given move list [moves] in the state
    [state]. [acc] is the accumulated return value*)
let rec all_valid_moves_helper color moves state acc =
  match moves with
  | [] -> acc
  | h :: t -> begin
      match h with
      | piece, orig_pos, new_pos ->
          let new_state =
            move_piece_helper piece color orig_pos new_pos state
          in
          if check_check (color |> opp_color) new_state then
            all_valid_moves_helper color t state acc
          else all_valid_moves_helper color t state (h :: acc)
    end

(**[all_valid_moves color state] is all the valid moves player with color
   [color] can make in board state [state]*)
let all_valid_moves color state =
  all_valid_moves_helper color
    (possible_moves color state (state |> get_board) [])
    state []

(**[check_checkmate color state] checks if color [color] has checkmated the
   other*)
let check_checkmate color state =
  check_check color state && all_valid_moves (color |> opp_color) state = []

(**[check_stalemate color state] checks if color is at a stalemate and can not
   make any moves*)
let check_stalemate color state =
  (not (check_check (color |> opp_color) state))
  && all_valid_moves color state = []

let promote_pawn color pos to_piece state =
  let new_state =
    {
      state with
      board =
        piece_helper ~moved:true pos to_piece color
        :: (state |> get_board |> List.remove_assoc pos);
    }
  in
  if check_check color new_state && check_checkmate color new_state then
    Checkmate new_state
  else if check_stalemate (opp_color color) new_state then Stalemate new_state
  else if check_check color new_state then Check new_state
  else if
    new_state |> get_fifty_move_rule = 50
    || new_state |> get_num_repetitions = 3
  then Draw new_state
  else Legal new_state

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
    let new_state =
      move_piece_helper piece color orig_pos new_pos cur_state
      |> update_num_repetitions 1 (cur_state |> get_old_boards)
    in
    if check_check (opp_color color) new_state then Illegal
    else
      let new_state =
        {
          new_state with
          old_boards = get_board cur_state :: get_old_boards cur_state;
        }
      in
      if check_check color new_state && check_checkmate color new_state then
        Checkmate new_state
      else if check_stalemate (opp_color color) new_state then
        Stalemate new_state
      else if check_check color new_state then Check new_state
      else if
        new_state |> get_fifty_move_rule = 50
        || new_state |> get_num_repetitions = 3
      then Draw new_state
      else if
        piece = Pawn
        && ((color = White && String.get new_pos 1 = '8')
           || (color = Black && String.get new_pos 1 = '1'))
      then PawnPromotion new_state
      else Legal new_state
