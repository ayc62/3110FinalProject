<<<<<<< HEAD
type piece_type =
=======
type piece =
>>>>>>> 571312e761631a2cc18ec41051ec10ead26958db
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  | Pawn

<<<<<<< HEAD
type piece_color =
  | White
  | Black

type piece = {
  piece_type : piece_type;
  piece_color : piece_color;
=======
type color =
  | White
  | Black

type piece_state = {
  pos : string;
  piece : piece;
  color : color;
>>>>>>> 571312e761631a2cc18ec41051ec10ead26958db
}
