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
end