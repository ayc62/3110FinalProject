exception InvalidPiece
exception InvalidSquare
exception InvalidCommand

open Board
open Check

type command =
  | Move of piece_type * string list
  | Resign

let parse_move (move_cmd : string list) =
  match move_cmd with
  | [] | [ _ ] | [ _; _ ] -> raise InvalidCommand
  | [ h1; h2; h3 ] ->
      let piece =
        if h1 = "Pawn" then Pawn
        else if h1 = "Knight" then Knight
        else if h1 = "Bishop" then Bishop
        else if h1 = "Queen" then Queen
        else if h1 = "King" then King
        else if h1 = "Rook" then Rook
        else raise InvalidPiece
      in
      if (not (check_square h2)) || not (check_square h3) then
        raise InvalidSquare
      else Move (piece, [ h2; h3 ])
  | _ -> raise InvalidCommand

let parse str =
  match
    str |> String.split_on_char ' '
    |> List.filter (fun x -> String.length x > 0)
  with
  | [] -> raise InvalidCommand
  | h :: t ->
      if h = "move" then parse_move t
      else if h = "resign" then Resign
      else raise InvalidCommand
