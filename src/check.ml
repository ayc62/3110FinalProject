open Board

let opp_color = function
  | White -> Black
  | Black -> White

(** [check_square square] checks if the square [square] is a valid square in the
    board*)
let check_square (square : string) : bool =
  if not (String.length square = 2) then false
  else if not ('a' <= String.get square 0 && String.get square 0 <= 'h') then
    false
  else if not ('1' <= String.get square 1 && String.get square 1 <= '8') then
    false
  else true

(**[get_column c r acc] are the squares on column [c] starting on row [r] and
   going down. Returns [acc]*)
let rec get_column c r acc =
  if r = "0" then acc
  else
    get_column c (r |> int_of_string |> ( + ) (-1) |> string_of_int) []
    @ ((c ^ r) :: acc)

(** [get_columns c acc] gets all the columns and adds to [acc]. Return [acc] at
    the end *)
let rec get_columns c acc =
  if c = "`" then acc
  else
    get_columns
      (String.get c 0 |> Char.code |> ( + ) (-1) |> Char.chr |> String.make 1)
      []
    @ get_column c "8" [] @ acc

(** [all_squares] is all the squares on the chess board*)
let all_squares = get_columns "h" []

(** [diff new_pos orig_pos index] is the difference in position between
    [orig_pos] and [new_pos] in index [index]. [index] is 0 for horizontal
    distance and 1 for vertical distance*)
let diff new_pos orig_pos index =
  Char.code (String.get new_pos index) - Char.code (String.get orig_pos index)

(** [move_horizontal dir pos] is the position [dir] horizontal squares away from
    square [pos] in the direction [dir]*)
let move_horizontal dir pos =
  String.get pos 1 |> Char.escaped
  |> ( ^ )
       (String.get pos 0 |> Char.code |> ( + ) dir |> Char.chr |> String.make 1)

(** [move_vertical dir pos] is the position [dir] vertical squares away from
    square [pos] in the direction [dir]*)
let move_vertical dir pos =
  String.get pos 1 |> Char.code |> ( + ) dir |> Char.chr |> String.make 1
  |> ( ^ ) (String.get pos 0 |> Char.escaped)

(** [is_horizontal orig_pos new_pos] checks if two pieces are in the same row.
    Requires: [orig_pos] and [new_pos] are valid squares on the board*)
let is_horizontal orig_pos new_pos =
  String.get new_pos 1 = String.get orig_pos 1

(** [check_horizontal orig_pos new_pos state] checks if there are any pieces
    between [orig_pos] and [new_pos] exclusive in the board [state], returning
    true if no pieces are in the way. Requires: [orig_pos] and [new_pos] are
    horizontal from each other and [state] is a valid state of the board *)
let rec check_horizontal orig_pos new_pos (state : state) =
  let diff = diff new_pos orig_pos 0 in
  if abs diff = 1 then true
  else
    let next_pos = move_horizontal (diff / abs diff) orig_pos in
    if state |> get_board |> List.assoc_opt next_pos = None then
      check_horizontal next_pos new_pos state
    else false

(** [is_vertical orig_pos new_pos] checks if two pieces are in the same column.
    Requires: [orig_pos] and [new_pos] are valid squares on the board*)
let is_vertical orig_pos new_pos = String.get new_pos 0 = String.get orig_pos 0

(** [check_vertical orig_pos new_pos state] checks if there are any pieces
    between [orig_pos] and [new_pos] exclusive in the board [state], returning
    true if no pieces are in the way. Requires: [orig_pos] and [new_pos] are
    vertical from each other and [state] is a valid state of the board *)
let rec check_vertical orig_pos new_pos (state : state) =
  let diff = diff new_pos orig_pos 1 in
  if abs diff = 1 then true
  else
    let next_pos = move_vertical (diff / abs diff) orig_pos in
    if state |> get_board |> List.assoc_opt next_pos = None then
      check_vertical next_pos new_pos state
    else false

(** [is_diagonal orig_pos new_pos] checks if two pieces are in the same
    diagonal. Requires: [orig_pos] and [new_pos] are valid squares on the board*)
let is_diagonal orig_pos new_pos =
  abs (diff new_pos orig_pos 0) = abs (diff new_pos orig_pos 1)

(** [check_diagonal orig_pos new_pos state] checks if there are any pieces
    between [orig_pos] and [new_pos] exclusive in the board [state], returning
    true if no pieces are in the way. Requires: [orig_pos] and [new_pos] are
    diagonal from each other and [state] is a valid state of the board *)
let rec check_diagonal orig_pos new_pos state =
  let column_diff = diff new_pos orig_pos 0 in
  if abs column_diff = 1 then true
  else
    let row_diff = diff new_pos orig_pos 1 in
    let next_pos =
      orig_pos
      |> move_horizontal (column_diff / abs column_diff)
      |> move_vertical (row_diff / abs row_diff)
    in
    if state |> get_board |> List.assoc_opt next_pos = None then
      check_diagonal next_pos new_pos state
    else false

(** [check_en_passant color orig_pos new_pos state] checks if en_passant is
    possible from [orig_pos] to [new_pos] for a pawn with color [color] in the
    current state [state]. *)
let check_en_passant color orig_pos new_pos state =
  try
    let dir = if color = White then 1 else -1 in
    let en_passant_pos = move_vertical (-1 * dir) new_pos in
    let start_pawn_pos = move_vertical dir new_pos in
    let piece = state |> get_board |> List.assoc en_passant_pos in
    get_piece_color piece <> color
    && state |> get_board |> List.assoc_opt start_pawn_pos = None
    &&
    match
      List.assoc_opt start_pawn_pos (state |> get_old_boards |> List.hd)
    with
    | None -> false
    | Some piece_state ->
        get_piece_color piece_state <> color
        && get_piece_type piece_state = Pawn
  with Not_found -> false

(** [check_pawn_attack color orig_pos new_pos state] checks if a pawn can
    capture a piece from [orig_pos] to [new_pos] for a pawn with color [color]
    in the current state [state]. *)
let check_pawn_attack color orig_pos new_pos state =
  let dir = if color = White then 1 else -1 in
  let x_dif = diff new_pos orig_pos 0 in
  let y_dif = diff new_pos orig_pos 1 in
  abs x_dif = 1
  && y_dif = dir
  && (begin
        match state |> get_board |> List.assoc_opt new_pos with
        | None -> false
        | Some piece -> get_piece_color piece <> color
      end
     || check_en_passant color orig_pos new_pos state)

(**[check_pawn color orig_pos new_pos state] checks if moving a pawn from
   [orig_pos] to [new_pos] is a valid move. Requires: [orig_pos] and [new_pos]
   are valid squares and [state] is a valid state of the board *)
let check_pawn color orig_pos new_pos state =
  let piece_state = state |> get_board |> List.assoc orig_pos in
  let dir = if color = White then 1 else -1 in
  let x_dif = diff new_pos orig_pos 0 in
  let y_dif = diff new_pos orig_pos 1 in
  if x_dif = 0 then
    if y_dif = dir && state |> get_board |> List.assoc_opt new_pos = None then
      true
    else
      (not (get_moved piece_state))
      && diff new_pos orig_pos 1 = 2 * dir
      && check_vertical orig_pos new_pos state
      && state |> get_board |> List.assoc_opt new_pos = None
  else
    abs x_dif = 1
    && y_dif = dir
    && check_pawn_attack color orig_pos new_pos state

(**[check_knight color orig_pos new_pos state] checks if moving a knight from
   [orig_pos] to [new_pos] is a valid move. Requires: [orig_pos] and [new_pos]
   are valid squares and [state] is a valid state of the board *)
let check_knight color orig_pos new_pos state =
  let x_dif = diff new_pos orig_pos 0 in
  let y_dif = diff new_pos orig_pos 1 in
  match (abs x_dif, abs y_dif) with
  | 1, 2 | 2, 1 -> begin
      match state |> get_board |> List.assoc_opt new_pos with
      | None -> true
      | Some piece_state -> get_piece_color piece_state <> color
    end
  | _ -> false

(**[check_bishop color orig_pos new_pos state] checks if moving a bishop from
   [orig_pos] to [new_pos] is a valid move. Requires: [orig_pos] and [new_pos]
   are valid squares and [state] is a valid state of the board *)
let check_bishop color orig_pos new_pos state =
  if is_diagonal orig_pos new_pos && check_diagonal orig_pos new_pos state then
    match state |> get_board |> List.assoc_opt new_pos with
    | None -> true
    | Some piece_state -> get_piece_color piece_state <> color
  else false

(**[check_rook color orig_pos new_pos state] checks if moving a rook from
   [orig_pos] to [new_pos] is a valid move. Requires: [orig_pos] and [new_pos]
   are valid squares and [state] is a valid state of the board *)
let check_rook color orig_pos new_pos state =
  if
    (is_horizontal orig_pos new_pos && check_horizontal orig_pos new_pos state)
    || (is_vertical orig_pos new_pos && check_vertical orig_pos new_pos state)
  then
    match state |> get_board |> List.assoc_opt new_pos with
    | None -> true
    | Some piece_state -> get_piece_color piece_state <> color
  else false

(**[check_queen color orig_pos new_pos state] checks if moving a queen from
   [orig_pos] to [new_pos] is a valid move. Requires: [orig_pos] and [new_pos]
   are valid squares and [state] is a valid state of the board *)
let check_queen color orig_pos new_pos state =
  if
    (is_horizontal orig_pos new_pos && check_horizontal orig_pos new_pos state)
    || (is_vertical orig_pos new_pos && check_vertical orig_pos new_pos state)
    || (is_diagonal orig_pos new_pos && check_diagonal orig_pos new_pos state)
  then
    match state |> get_board |> List.assoc_opt new_pos with
    | None -> true
    | Some piece_state -> get_piece_color piece_state <> color
  else false

(** [check_king_attack color orig_pos new_pos state] checks if a king with color
    [color] at squrae [orig_pos] attacks the square [new_pos] in the current
    state [state]. *)
let check_king_attack color orig_pos new_pos state =
  let x_dif = diff new_pos orig_pos 0 in
  let y_dif = diff new_pos orig_pos 1 in
  abs x_dif <= 1
  && abs y_dif <= 1
  &&
  match state |> get_board |> List.assoc_opt new_pos with
  | None -> true
  | Some piece_state -> color <> get_piece_color piece_state

(** [can_see piece color pos] checks if a piece with piece_type [piece] and
    color [color] can see the square [attack_pos]*)
let can_see piece color attack_pos state =
  piece |> snd |> get_piece_color = color
  &&
  match piece |> snd |> get_piece_type with
  | Pawn -> check_pawn_attack color (fst piece) attack_pos state
  | Knight -> check_knight color (fst piece) attack_pos state
  | Bishop -> check_bishop color (fst piece) attack_pos state
  | Rook -> check_rook color (fst piece) attack_pos state
  | Queen -> check_queen color (fst piece) attack_pos state
  | King -> check_king_attack color (fst piece) attack_pos state

(** [check_attack color pos state] checks if any piece with color [color]
    attacks position [pos]in state [state]*)
let rec check_attack color pos state board =
  match board with
  | [] -> false
  | h :: t ->
      if can_see h color pos state then true else check_attack color pos state t

(**[castle_rook col row dir state] is the rook that the king is castling with in
   the current state [state]*)
let rec castle_rook col row dir state =
  if col = "`" || col = "i" then None
  else
    match state |> get_board |> List.assoc_opt (col ^ row) with
    | None ->
        castle_rook
          (String.get col 0 |> Char.code |> ( + ) dir |> Char.chr
         |> String.make 1)
          row dir state
    | Some piece_state ->
        if get_piece_type piece_state = Rook && get_moved piece_state = false
        then Some (col ^ row)
        else None

(** [check_castle color orig_pos new_pos state] checks if castling is possible
    for side [color] with the King moving from [orig_pos] to [new_pos] in the
    current state [state]*)
let check_castle color orig_pos new_pos state =
  let piece_state = state |> get_board |> List.assoc orig_pos in
  if
    abs (diff new_pos orig_pos 0) = 2
    && state |> get_board |> List.assoc_opt new_pos = None
    && get_moved piece_state = false
  then
    let dir = diff new_pos orig_pos 0 / abs (diff new_pos orig_pos 0) in
    let col =
      String.get orig_pos 0 |> Char.code |> ( + ) dir |> Char.chr
      |> String.make 1
    in
    let row = String.get orig_pos 1 |> String.make 1 in
    match castle_rook col row dir state with
    | None -> false
    | Some _ ->
        (not
           (check_attack (color |> opp_color) orig_pos state (state |> get_board)))
        && (not
              (check_attack (color |> opp_color)
                 (orig_pos |> move_horizontal dir)
                 state (state |> get_board)))
        && not
             (check_attack (color |> opp_color)
                (orig_pos |> move_horizontal dir |> move_horizontal dir)
                state (state |> get_board))
  else false

(**[check_king color orig_pos new_pos state] checks if moving a king from
   [orig_pos] to [new_pos] is a valid move. Requires: [orig_pos] and [new_pos]
   are valid squares and [state] is a valid state of the board *)
let check_king color orig_pos new_pos state =
  check_king_attack color orig_pos new_pos state
  && state |> get_board
     |> check_attack (color |> opp_color) new_pos state
     |> not
  || check_castle color orig_pos new_pos state

(** [check_piece_move piece color orig_pos new_pos state] checks if the piece
    [piece] of color [color] can move (based solely on piece restriction, and
    not other rules) from square [orig_pos] to square [new_pos] in the current
    state [state]*)
let check_piece_move piece color orig_pos new_pos state =
  match state |> get_board |> List.assoc_opt orig_pos with
  | None -> false
  | Some piece_state -> (
      piece = get_piece_type piece_state
      && color = get_piece_color piece_state
      &&
      match piece with
      | Pawn -> check_pawn color orig_pos new_pos state
      | Knight -> check_knight color orig_pos new_pos state
      | Bishop -> check_bishop color orig_pos new_pos state
      | Rook -> check_rook color orig_pos new_pos state
      | Queen -> check_queen color orig_pos new_pos state
      | King -> check_king color orig_pos new_pos state)

(** [king_square color state] gets the current position of the king with color
    [color] in the current state. Raises: Not_found if there is no king*)
let rec king_square color state = function
  | [] -> raise Not_found
  | h :: t -> (
      match List.assoc_opt h (get_board state) with
      | None -> king_square color state t
      | Some piece_state ->
          if
            get_piece_color piece_state = color
            && get_piece_type piece_state = King
          then h
          else king_square color state t)

(** [check_check color state] checks if color [color] is checking the opposing
    kick in the current state [state]*)
let check_check color state =
  let king_square = king_square (color |> opp_color) state all_squares in
  check_attack color king_square state (get_board state)

(** [move_list_helper piece orig_pos possible_new_pos acc] is a helper that
    generates a list of moves. The return format is
    [...; (piece, orig_pos, new_pos); ...;]*)
let rec moves_list_helper piece orig_pos possible_new_pos acc =
  match possible_new_pos with
  | [] -> acc
  | h :: t ->
      let acc =
        (if check_square h then [ (piece, orig_pos, h) ] else []) @ acc
      in
      moves_list_helper piece orig_pos t acc

(** [all_pawn_moves color orig_pos state] is all the squares a pawn on square
    [orig_pos] can move to. Does not need to be a valid move*)
let all_pawn_moves color orig_pos state =
  let dir = if color = White then 1 else -1 in
  let for_pos = orig_pos |> move_vertical dir in
  moves_list_helper Pawn orig_pos
    [
      for_pos;
      for_pos |> move_vertical dir;
      for_pos |> move_horizontal (-1);
      for_pos |> move_horizontal 1;
    ]
    []

(** [all_knight_moves color orig_pos state] is all the squares a pawn on square
    [orig_pos] can move to. Does not need to be a valid move*)
let all_knight_moves color orig_pos state =
  moves_list_helper Knight orig_pos
    [
      orig_pos |> move_horizontal 1 |> move_vertical 2;
      orig_pos |> move_horizontal 1 |> move_vertical (-2);
      orig_pos |> move_horizontal (-1) |> move_vertical 2;
      orig_pos |> move_horizontal (-1) |> move_vertical (-2);
      orig_pos |> move_horizontal 2 |> move_vertical 1;
      orig_pos |> move_horizontal 2 |> move_vertical (-1);
      orig_pos |> move_horizontal (-2) |> move_vertical 1;
      orig_pos |> move_horizontal (-2) |> move_vertical (-1);
    ]
    []

(** [continuous_moves pos hor_dir vert_dir board acc] is a list of continuous
    moves from position [pos] going in direction [hor_dir], [vert_dir] in the
    board [board]. [acc] is the accumulated list of positions*)
let rec continuous_moves pos hor_dir vert_dir board acc =
  let new_pos = pos |> move_horizontal hor_dir |> move_vertical vert_dir in
  if board |> List.assoc_opt pos = None && check_square new_pos then
    new_pos :: continuous_moves new_pos hor_dir vert_dir board acc
  else acc

(** [all_bishop_moves color orig_pos state] is all the squares a pawn on square
    [orig_pos] can move to. Does not need to be a valid move*)
let all_bishop_moves color orig_pos state =
  moves_list_helper Bishop orig_pos
    (continuous_moves orig_pos 1 1 (get_board state) []
    @ continuous_moves orig_pos 1 (-1) (get_board state) []
    @ continuous_moves orig_pos (-1) 1 (get_board state) []
    @ continuous_moves orig_pos (-1) (-1) (get_board state) [])
    []

(** [all_rook_moves color orig_pos state] is all the squares a pawn on square
    [orig_pos] can move to. Does not need to be a valid move*)
let all_rook_moves color orig_pos state =
  moves_list_helper Bishop orig_pos
    (continuous_moves orig_pos 1 0 (get_board state) []
    @ continuous_moves orig_pos (-1) 0 (get_board state) []
    @ continuous_moves orig_pos 0 1 (get_board state) []
    @ continuous_moves orig_pos 0 (-1) (get_board state) [])
    []

(** [all_queen_moves color orig_pos state] is all the squares a pawn on square
    [orig_pos] can move to. Does not need to be a valid move*)
let all_queen_moves color orig_pos state =
  all_bishop_moves color orig_pos state @ all_rook_moves color orig_pos state

(** [all_king_moves color orig_pos state] is all the squares a pawn on square
    [orig_pos] can move to. Does not need to be a valid move*)
let all_king_moves color orig_pos state =
  moves_list_helper King orig_pos
    [
      orig_pos |> move_horizontal 1 |> move_vertical 1;
      orig_pos |> move_horizontal 1 |> move_vertical (-1);
      orig_pos |> move_horizontal (-1) |> move_vertical 2;
      orig_pos |> move_horizontal (-1) |> move_vertical (-2);
      orig_pos |> move_horizontal 2 |> move_vertical 1;
      orig_pos |> move_horizontal 2 |> move_vertical (-1);
      orig_pos |> move_horizontal (-2) |> move_vertical 1;
      orig_pos |> move_horizontal (-2) |> move_vertical (-1);
    ]
    []

(** [filter_moves color orig_pos state] is all the moves a pawn on square
    [orig_pos] can make in the current state [state]*)
let rec filter_moves color state moves acc =
  match moves with
  | [] -> acc
  | h :: t -> begin
      match h with
      | piece_type, orig_pos, new_pos ->
          let check =
            match piece_type with
            | Pawn -> check_pawn
            | Knight -> check_knight
            | Bishop -> check_bishop
            | Rook -> check_rook
            | Queen -> check_queen
            | King -> check_king
          in
          if check color orig_pos new_pos state then
            filter_moves color state t (h :: acc)
          else filter_moves color state t acc
    end

(** [possible_moves color state board acc] is all the possible moves color
    [color] could make in the current state*)
let rec possible_moves color state board acc =
  match board with
  | [] -> filter_moves color state acc []
  | h :: t ->
      if h |> snd |> get_piece_color = color then
        let piece_moves =
          match h |> snd |> get_piece_type with
          | Pawn -> all_pawn_moves color (fst h) state
          | Knight -> all_knight_moves color (fst h) state
          | Bishop -> all_bishop_moves color (fst h) state
          | Rook -> all_rook_moves color (fst h) state
          | Queen -> all_queen_moves color (fst h) state
          | King -> all_king_moves color (fst h) state
        in
        possible_moves color state t (piece_moves @ acc)
      else possible_moves color state t acc
