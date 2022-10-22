open Board

exception OccupiedSquare of string
exception InvalidMove of string

let opp_color = function
  | White -> Black
  | Black -> White

let check_square (square : string) : bool =
  if not (String.length square = 2) then false
  else if not ('a' <= String.get square 0 && String.get square 0 <= 'h') then
    false
  else if not ('1' <= String.get square 1 && String.get square 1 <= '8') then
    false
  else true

let rec get_column c r acc =
  if r = "0" then acc
  else
    get_column c (r |> int_of_string |> ( + ) (-1) |> string_of_int) []
    @ ((c ^ r) :: acc)

(** [get_columns] is all the squares on the chess board*)
let rec get_columns c acc =
  if c = "`" then acc
  else
    get_columns
      (String.get c 0 |> Char.code |> ( + ) (-1) |> Char.chr |> String.make 1)
      []
    @ get_column c "8" [] @ acc

(** [get_squares] is all the squares on the chess board*)
let get_squares = get_columns "h" []

(** [diff new_pos orig_pos index] is the difference in position between
    [orig_pos] and [new_pos] in index [index]. [index] is 0 for horizontal
    distance and 1 for vertical distance*)
let diff new_pos orig_pos index =
  Char.code (String.get new_pos index) - Char.code (String.get orig_pos index)

(** [move_horizontal dir pos] is the position 1 horizontal square away from
    square [pos] in the direction [dir]*)
let move_horizontal dir pos =
  String.get pos 1 |> Char.escaped
  |> ( ^ )
       (String.get pos 0 |> Char.code |> ( + ) dir |> Char.chr |> String.make 1)

(** [move_vertical dir pos] is the position 1 vertical square away from square
    [pos] in the direction [dir]*)
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
  abs x_dif <= 1 && y_dif <= 2

(** [can_see piece color pos] checks if a piece with piece_type [piece] and
    color [color] can see the square [pos]*)
let can_see piece color attack_pos state =
  match piece |> snd |> get_piece_type with
  | Pawn -> check_pawn_attack color (fst piece) attack_pos state
  | Knight -> check_knight color (fst piece) attack_pos state
  | Bishop -> check_bishop color (fst piece) attack_pos state
  | Rook -> check_rook color (fst piece) attack_pos state
  | Queen -> check_queen color (fst piece) attack_pos state
  | King -> check_king_attack color (fst piece) attack_pos state

(** [check_attacked color pos state] checks if a position [pos] is attacked by
    any piece with color [color] in state [state]*)
let rec check_attacked color pos state board =
  match board with
  | [] -> true
  | h :: t ->
      if can_see h color pos state then false
      else check_attacked color pos state t

let rec get_castle_rook col row dir state =
  if col = "`" || col = "i" then None
  else
    match state |> get_board |> List.assoc_opt (col ^ row) with
    | None ->
        get_castle_rook
          (String.get col 0 |> Char.code |> ( + ) dir |> Char.chr
         |> String.make 1)
          row dir state
    | Some piece_state ->
        if get_piece_type piece_state = Rook && get_moved piece_state = false
        then Some (col ^ row)
        else None

let check_castle color orig_pos new_pos state =
  let piece_state = state |> get_board |> List.assoc orig_pos in
  let dir = diff new_pos orig_pos 0 / abs (diff new_pos orig_pos 0) in
  let col =
    String.get orig_pos 0 |> Char.code |> ( + ) dir |> Char.chr |> String.make 1
  in
  let row = String.get orig_pos 1 |> String.make 1 in
  get_moved piece_state = false
  &&
  match get_castle_rook col row dir state with
  | None -> false
  | Some _ ->
      check_attacked (color |> opp_color)
        (orig_pos |> move_horizontal dir)
        state (state |> get_board)
      |> not
      && check_attacked (color |> opp_color)
           (orig_pos |> move_horizontal dir |> move_horizontal dir)
           state (state |> get_board)
         |> not

(**[check_king color orig_pos new_pos state] checks if moving a king from
   [orig_pos] to [new_pos] is a valid move. Requires: [orig_pos] and [new_pos]
   are valid squares and [state] is a valid state of the board *)
let check_king color orig_pos new_pos state =
  state |> get_board |> check_attacked (color |> opp_color) new_pos state |> not
  && check_king_attack color orig_pos new_pos state
  || abs (diff new_pos orig_pos 0) = 2
     && check_castle color orig_pos new_pos state

let check_piece_move piece color orig_pos new_pos state =
  match state |> get_board |> List.assoc_opt orig_pos with
  | None -> false
  | Some piece_state -> begin
      begin
        piece = get_piece_type piece_state
        &&
        match piece with
        | Pawn -> check_pawn color orig_pos new_pos state
        | Knight -> check_knight color orig_pos new_pos state
        | Bishop -> check_bishop color orig_pos new_pos state
        | Rook -> check_rook color orig_pos new_pos state
        | Queen -> check_queen color orig_pos new_pos state
        | King -> check_king color orig_pos new_pos state
      end
    end

let check_check = false
let check_check_block = true
let check_checkmate = false

let check_valid_move piece color orig_pos new_pos state =
  if not check_check then check_piece_move piece color orig_pos new_pos state
  else check_check_block
