(** All chess rules/logic implemented here *)

val opp_color : Board.piece_color -> Board.piece_color
(**[opp_color color] returns the opposite color of [color]*)

val check_square : string -> bool
(** [check_square square] checks if the square [square] is a valid square in the
    board*)

val diff : string -> string -> int -> int
(** [diff new_pos orig_pos index] is the difference in position between
    [orig_pos] and [new_pos] in index [index]. [index] is 0 for horizontal
    distance and 1 for vertical distance*)

val move_horizontal : int -> string -> string
(** [move_horizontal dir pos] is the position [dir] horizontal squares away from
    square [pos] in the direction [dir]*)

val move_vertical : int -> string -> string
(** [move_vertical dir pos] is the position [dir] vertical squares away from
    square [pos] in the direction [dir]*)

val is_horizontal : string -> string -> bool
(** [is_horizontal orig_pos
  new_pos] checks if two pieces are in the same row.
    Requires: [orig_pos] and [new_pos] are valid squares on the board*)

val check_horizontal : string -> string -> Board.state -> bool
(** [check_horizontal orig_pos new_pos state] checks if there are any pieces
    between [orig_pos] and [new_pos] exclusive in the board [state], returning
    true if no pieces are in the way. Requires: [orig_pos] and [new_pos] are
    horizontal from each other and [state] is a valid state of the board *)

val is_vertical : string -> string -> bool
(** [is_vertical orig_pos new_pos] checks if two pieces are in the same column.
    Requires: [orig_pos] and [new_pos] are valid squares on the board*)

val check_vertical : string -> string -> Board.state -> bool
(** [check_vertical orig_pos new_pos state] checks if there are any pieces
    between [orig_pos] and [new_pos] exclusive in the board [state], returning
    true if no pieces are in the way. Requires: [orig_pos] and [new_pos] are
    vertical from each other and [state] is a valid state of the board *)

val is_diagonal : string -> string -> bool
(** [is_diagonal orig_pos new_pos] checks if two pieces are in the same
    diagonal. Requires: [orig_pos] and [new_pos] are valid squares on the board*)

val check_diagonal : string -> string -> Board.state -> bool
(** [check_diagonal orig_pos new_pos state] checks if there are any pieces
    between [orig_pos] and [new_pos] exclusive in the board [state], returning
    true if no pieces are in the way. Requires: [orig_pos] and [new_pos] are
    diagonal from each other and [state] is a valid state of the board *)

val check_en_passant : Board.piece_color -> 'a -> string -> Board.state -> bool
(** [check_en_passant color orig_pos new_pos state] checks if en_passant is
    possible from [orig_pos] to [new_pos] for a pawn with color [color] in the
    current state [state]. *)

val castle_rook : string -> int -> Board.state -> string option
(**[castle_rook pos dir state] is the rook that the king is castling with in the
   current state [state]*)

val check_castle : Board.piece_color -> string -> string -> Board.state -> bool
(** [check_castle color orig_pos new_pos state] checks if castling is possible
    for side [color] with the King moving from [orig_pos] to [new_pos] in the
    current state [state]*)

val check_piece_move :
  Board.piece_type ->
  Board.piece_color ->
  string ->
  string ->
  Board.state ->
  bool
(** [check_piece_move piece color orig_pos new_pos state] checks if the piece
    [piece] of color [color] can move (based solely on piece restriction, and
    not other rules) from square [orig_pos] to square [new_pos] in the current
    state [state]*)

val check_check : Board.piece_color -> Board.state -> bool
(** [check_check color state] checks if color [color] is checking the opposing
    kick in the current state [state]*)

val possible_moves :
  Board.piece_color ->
  Board.state ->
  (string * Board.piece_state) list ->
  (Board.piece_type * string * string) list ->
  (Board.piece_type * string * string) list
(** [possible_moves color state board acc] is all the possible moves color
    [color] could make in the current state*)
