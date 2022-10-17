<<<<<<< HEAD
=======
(* Note: You may introduce new code anywhere in this file. *)
>>>>>>> 571312e761631a2cc18ec41051ec10ead26958db
open Board

let init_state =
  [
<<<<<<< HEAD
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
=======
    { pos = "a1"; piece = Rook; color = White };
    { pos = "b1"; piece = Knight; color = White };
    { pos = "c1"; piece = Bishop; color = White };
    { pos = "d1"; piece = Queen; color = White };
    { pos = "e1"; piece = King; color = White };
    { pos = "f1"; piece = Bishop; color = White };
    { pos = "g1"; piece = Knight; color = White };
    { pos = "h1"; piece = Rook; color = White };
    { pos = "a2"; piece = Pawn; color = White };
    { pos = "b2"; piece = Pawn; color = White };
    { pos = "c2"; piece = Pawn; color = White };
    { pos = "d2"; piece = Pawn; color = White };
    { pos = "e2"; piece = Pawn; color = White };
    { pos = "f2"; piece = Pawn; color = White };
    { pos = "g2"; piece = Pawn; color = White };
    { pos = "h2"; piece = Pawn; color = White };
    { pos = "a8"; piece = Rook; color = Black };
    { pos = "b8"; piece = Knight; color = Black };
    { pos = "c8"; piece = Bishop; color = Black };
    { pos = "d8"; piece = Queen; color = Black };
    { pos = "e8"; piece = King; color = Black };
    { pos = "f8"; piece = Bishop; color = Black };
    { pos = "g8"; piece = Knight; color = Black };
    { pos = "h8"; piece = Rook; color = Black };
    { pos = "a7"; piece = Pawn; color = Black };
    { pos = "b7"; piece = Pawn; color = Black };
    { pos = "c7"; piece = Pawn; color = Black };
    { pos = "d7"; piece = Pawn; color = Black };
    { pos = "e7"; piece = Pawn; color = Black };
    { pos = "f7"; piece = Pawn; color = Black };
    { pos = "g7"; piece = Pawn; color = Black };
    { pos = "h7"; piece = Pawn; color = Black };
  ]
>>>>>>> 571312e761631a2cc18ec41051ec10ead26958db
