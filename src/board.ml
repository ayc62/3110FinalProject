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

type square_color =
  | Light
  | Dark

type piece_state = {
  piece_type : piece_type;
  piece_color : piece_color;
  moved : bool;
}

(**[piece_helper] takes in the position [pos] of a piece, and the type [p_type]
   and color [p_color] of the piece, and returns it in an association list entry
   format. [pos] is the key, and the value is a record with [p_type] and
   [p_color].*)
let piece_helper ?(moved = false) (pos : string) piece_type piece_color =
  (pos, { piece_type; piece_color; moved })

let get_piece_type piece_state = piece_state.piece_type
let get_piece_color piece_state = piece_state.piece_color
let get_moved piece_state = piece_state.moved

type board = (string * piece_state) list

type state = {
  board : (string * piece_state) list;
  old_boards : board list;
  fifty_move_rule : int;
  captured_pieces : piece_state list;
  num_repetition : int;
}

let get_board state = state.board
let get_old_boards state = state.old_boards
let get_fifty_move_rule state = state.fifty_move_rule
let get_captured_pieces state = state.captured_pieces
let get_num_repetitions state = state.num_repetition

(**[init_state] returns the initial state of the chessboard before any moves
   have been made. The initial state of the chessboard is the standard setup for
   a chessboard.*)
let init_state : state =
  {
    board =
      [
        piece_helper "a1" Rook White;
        piece_helper "b1" Knight White;
        piece_helper "c1" Bishop White;
        piece_helper "d1" Queen White;
        piece_helper "e1" King White;
        piece_helper "f1" Bishop White;
        piece_helper "g1" Knight White;
        piece_helper "h1" Rook White;
        piece_helper "a2" Pawn White;
        piece_helper "b2" Pawn White;
        piece_helper "c2" Pawn White;
        piece_helper "d2" Pawn White;
        piece_helper "e2" Pawn White;
        piece_helper "f2" Pawn White;
        piece_helper "g2" Pawn White;
        piece_helper "h2" Pawn White;
        piece_helper "a7" Pawn Black;
        piece_helper "b7" Pawn Black;
        piece_helper "c7" Pawn Black;
        piece_helper "d7" Pawn Black;
        piece_helper "e7" Pawn Black;
        piece_helper "f7" Pawn Black;
        piece_helper "g7" Pawn Black;
        piece_helper "h7" Pawn Black;
        piece_helper "a8" Rook Black;
        piece_helper "b8" Knight Black;
        piece_helper "c8" Bishop Black;
        piece_helper "d8" Queen Black;
        piece_helper "e8" King Black;
        piece_helper "f8" Bishop Black;
        piece_helper "g8" Knight Black;
        piece_helper "h8" Rook Black;
      ];
    old_boards = [];
    captured_pieces = [];
    fifty_move_rule = 0;
    num_repetition = 1;
  }

let possible_pos = [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" |]

let int_to_pos color i =
  if color = White then possible_pos.(i) ^ "1" else possible_pos.(i) ^ "8"

let gen_bishop_pos color sq_color =
  if (sq_color = Dark && color = White) || (sq_color = Light && color = Black)
  then int_to_pos color (Random.int 4 * 2)
  else int_to_pos color ((Random.int 4 * 2) + 1)

let rec gen_rand_pos color taken =
  let pos = int_to_pos color (Random.int 9) in
  if List.exists (fun x -> x = pos) taken then gen_rand_pos color taken else pos

let difference (lst1 : string list) (lst2 : string list) =
  List.filter (fun elt -> not (List.mem elt lst2)) lst1

let fischer_random_pieces color arr =
  let lst1 =
    [
      (Bishop, gen_bishop_pos color Light); (Bishop, gen_bishop_pos color Dark);
    ]
  in
  let taken1 = snd (List.split lst1) in
  let lst2 = (Queen, gen_rand_pos color taken1) :: lst1 in
  let taken2 = snd (List.split lst2) in
  let lst3 = (Knight, gen_rand_pos color taken2) :: lst2 in
  let taken3 = snd (List.split lst3) in
  let lst4 = (Knight, gen_rand_pos color taken3) :: lst3 in
  let taken4 = snd (List.split lst4) in
  let diff = difference arr taken4 in
  (Rook, List.nth diff 2)
  :: (King, List.nth diff 1)
  :: (Rook, List.hd diff)
  :: lst4

let gen_fischer_random_pos color : state =
  let w_pieces =
    fischer_random_pieces White
      [ "a1"; "b1"; "c1"; "d1"; "e1"; "f1"; "g1"; "h1" ]
  in
  let b_pieces =
    fischer_random_pieces Black
      [ "a8"; "b8"; "c8"; "d8"; "e8"; "f8"; "g8"; "h8" ]
  in
  failwith ""

let square_has_pt (state : state) (square : string) (piece_type : piece_type)
    (piece_color : piece_color) =
  try
    let square_piece = List.assoc square state.board in
    square_piece.piece_type = piece_type
    && square_piece.piece_color = piece_color
  with Not_found -> false
