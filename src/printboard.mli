(** Visualization of the chessboard for the players *)

val print_board_white : Board.state -> Command.variant -> Command.rounds -> unit
(** [print_board_white state variant rounds] prints the chessboard from White's
    point of view.*)

val print_board_black : Board.state -> Command.variant -> Command.rounds -> unit
(** [print_board_black state variant rounds] prints the chessboard from Black's
    point of view.*)
