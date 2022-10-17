type piece_type =
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  | Pawn

type piece_color =
  | White
  | Black

type piece = {
  piece_type : piece_type;
  piece_color : piece_color;
}
