open Board

exception OccupiedSquare of string
exception InvalidMove of string

(** [is_horizontal orig_pos new_pos] checks if two pieces are in the same row.
    Requires: [orig_pos] and [new_pos] are valid squares on the board*)
let is_horizontal orig_pos new_pos =
  String.get new_pos 1 = String.get orig_pos 1

(** [check_horizontal orig_pos new_pos state] checks if there are any pieces
    between [orig_pos] and [new_pos] exclusive in the board [state], returning
    true if no pieces are in the way. Requires: [orig_pos] and [new_pos] are
    horizontal from each other and [state] is a valid state of the board *)
let rec check_horizontal orig_pos new_pos (state : state) =
  let diff =
    Char.code (String.get new_pos 0) - Char.code (String.get orig_pos 0)
  in
  if abs diff = 1 then true
  else
    let next_pos =
      String.get orig_pos 1 |> Char.escaped
      |> ( ^ )
           (String.get orig_pos 0 |> Char.code
           |> ( + ) (diff / abs diff)
           |> Char.chr |> String.make 1)
    in
    if List.assoc_opt next_pos (get_board state) = None then
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
  let diff =
    Char.code (String.get new_pos 1) - Char.code (String.get orig_pos 1)
  in
  if abs diff = 1 then true
  else
    let next_pos =
      String.get orig_pos 1 |> Char.code
      |> ( + ) (diff / abs diff)
      |> Char.chr |> String.make 1
      |> ( ^ ) (String.get orig_pos 0 |> Char.escaped)
    in
    if List.assoc_opt next_pos (get_board state) = None then
      check_vertical next_pos new_pos state
    else false

(** [is_diagonal orig_pos new_pos] checks if two pieces are in the same
    diagonal. Requires: [orig_pos] and [new_pos] are valid squares on the board*)
let is_diagonal orig_pos new_pos =
  abs (Char.code (String.get new_pos 0) - Char.code (String.get orig_pos 0))
  = abs (Char.code (String.get new_pos 1) - Char.code (String.get orig_pos 1))

(** [check_diagonal orig_pos new_pos state] checks if there are any pieces
    between [orig_pos] and [new_pos] exclusive in the board [state], returning
    true if no pieces are in the way. Requires: [orig_pos] and [new_pos] are
    diagonal from each other and [state] is a valid state of the board *)
let rec check_diagonal orig_pos new_pos state =
  let column_diff =
    Char.code (String.get new_pos 0) - Char.code (String.get orig_pos 0)
  in
  if abs column_diff = 1 then true
  else
    let row_diff =
      Char.code (String.get new_pos 1) - Char.code (String.get orig_pos 1)
    in
    let next_pos =
      (String.get orig_pos 0 |> Char.code
      |> ( + ) (column_diff / abs column_diff)
      |> Char.chr |> String.make 1)
      ^ (String.get orig_pos 1 |> Char.code
        |> ( + ) (row_diff / abs row_diff)
        |> Char.chr |> String.make 1)
    in
    if List.assoc_opt next_pos (get_board state) = None then
      check_diagonal next_pos new_pos state
    else false

let check_en_passant color orig_pos new_pos dir state =
  let en_passant_pos =
    1 |> String.get orig_pos |> Char.escaped
    |> ( ^ ) (0 |> String.get new_pos |> Char.escaped)
  in
  let start_pawn_pos =
    1 |> String.get new_pos |> int_of_char
    |> ( + ) (dir - 48)
    |> string_of_int
    |> ( ^ ) (0 |> String.get new_pos |> Char.escaped)
  in
  let piece = List.assoc en_passant_pos (get_board state) in
  get_piece_color piece <> color
  && List.assoc_opt start_pawn_pos (get_board state) = None
  &&
  match List.assoc_opt start_pawn_pos (state |> get_old_boards |> List.hd) with
  | None -> false
  | Some { piece_color; piece_type; moved } ->
      piece_color <> color && piece_type = Pawn

(**[check_pawn color orig_pos new_pos state] checks if moving a pawn from
   [orig_pos] to [new_pos] is a valid move. Requires: [orig_pos] and [new_pos]
   are valid squares and [state] is a valid state of the board *)
let check_pawn color orig_pos new_pos (state : state) =
  match List.assoc_opt orig_pos (get_board state) with
  | None -> false
  | Some piece_state ->
      let dir = if color = White then 1 else -1 in
      let x_dif =
        Char.code (String.get new_pos 0) - Char.code (String.get orig_pos 0)
      in
      let y_dif =
        Char.code (String.get new_pos 1) - Char.code (String.get orig_pos 1)
      in
      if x_dif = 0 then
        if y_dif = dir && List.assoc_opt new_pos (get_board state) = None then
          true
        else
          (not (get_moved piece_state))
          && check_vertical orig_pos new_pos state
          && List.assoc_opt new_pos (get_board state) = None
      else
        abs x_dif = 1
        && y_dif = dir
        && (begin
              match List.assoc_opt new_pos (get_board state) with
              | None -> false
              | Some piece -> get_piece_color piece <> color
            end
           || check_en_passant color orig_pos new_pos dir state)

let check_knight = true
let check_bishop = true
let check_rook = true
let check_queen = true
let check_king = true

let can_see piece color pos =
  let piece_state = snd piece in
  match get_piece_type piece_state with
  | Rook -> true
  | Knight -> true
  | Bishop -> true
  | Queen -> true
  | King -> true
  | Pawn -> true

let rec check_attacked color pos state =
  match state with
  | [] -> true
  | h :: t -> if can_see h color pos then false else check_attacked color pos t
