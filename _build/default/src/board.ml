module Board = struct
  type piece =
    | Rook
    | Knight
    | Bishop
    | Queen
    | King
    | Pawn

  type color =
    | White
    | Black

  type piece_state = {
    pos : string;
    piece : piece;
    color : color;
  }

  let init_state =
    [
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
end