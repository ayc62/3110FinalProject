(* Note: You may introduce new code anywhere in this file. *)
open Board

let init_state =
  [
    ("a1", { piece_type = Rook; piece_color = White });
    ("b1", { piece_type = Knight; piece_color = White });
    ("c1", { piece_type = Bishop; piece_color = White });
    ("d1", { piece_type = Queen; piece_color = White });
    ("e1", { piece_type = King; piece_color = White });
    ("f1", { piece_type = Bishop; piece_color = White });
    ("g1", { piece_type = Knight; piece_color = White });
    ("h1", { piece_type = Rook; piece_color = White });
    ("a2", { piece_type = Pawn; piece_color = White });
    ("b2", { piece_type = Pawn; piece_color = White });
    ("c2", { piece_type = Pawn; piece_color = White });
    ("d2", { piece_type = Pawn; piece_color = White });
    ("e2", { piece_type = Pawn; piece_color = White });
    ("f2", { piece_type = Pawn; piece_color = White });
    ("g2", { piece_type = Pawn; piece_color = White });
    ("h2", { piece_type = Pawn; piece_color = White });
    ("a7", { piece_type = Pawn; piece_color = Black });
    ("b7", { piece_type = Pawn; piece_color = Black });
    ("c7", { piece_type = Pawn; piece_color = Black });
    ("d7", { piece_type = Pawn; piece_color = Black });
    ("e7", { piece_type = Pawn; piece_color = Black });
    ("f7", { piece_type = Pawn; piece_color = Black });
    ("g7", { piece_type = Pawn; piece_color = Black });
    ("h7", { piece_type = Pawn; piece_color = Black });
    ("a8", { piece_type = Rook; piece_color = Black });
    ("b8", { piece_type = Knight; piece_color = Black });
    ("c8", { piece_type = Bishop; piece_color = Black });
    ("d8", { piece_type = Queen; piece_color = Black });
    ("e8", { piece_type = King; piece_color = Black });
    ("f8", { piece_type = Bishop; piece_color = Black });
    ("g8", { piece_type = Knight; piece_color = Black });
    ("h8", { piece_type = Rook; piece_color = Black });
  ]
