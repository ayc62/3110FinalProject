exception InvalidPiece
exception InvalidSquare
exception InvalidCommand
exception InvalidVariant
exception InvalidResponse

open Board
open Check

type command =
  | Move of piece_type * string list
  | Resign
  | DrawOffer

type variant =
  | Standard
  | ThreeCheck
  | KingOfTheHill

type response =
  | Yes
  | No

let piece_match x =
  if x = "Pawn" then Pawn
  else if x = "Knight" then Knight
  else if x = "Bishop" then Bishop
  else if x = "Queen" then Queen
  else if x = "King" then King
  else if x = "Rook" then Rook
  else raise InvalidPiece

let variant_match x =
  if x = "Standard" then Standard
  else if x = "3-check" then ThreeCheck
  else if x = "KOTH" then KingOfTheHill
  else raise InvalidVariant

let parse_move (move_cmd : string list) =
  match move_cmd with
  | [] | [ _ ] | [ _; _ ] -> raise InvalidCommand
  | [ h1; h2; h3 ] ->
      let piece = piece_match h1 in
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
      else if h = "draw" then DrawOffer
      else if h = "resign" then Resign
      else raise InvalidCommand

let parse_promotion str =
  match
    str |> String.split_on_char ' '
    |> List.filter (fun x -> String.length x > 0)
  with
  | [ h ] -> (
      let matched = piece_match h in
      match matched with
      | Pawn -> raise InvalidPiece
      | _ -> matched)
  | _ -> raise InvalidPiece

let parse_variant str =
  match
    str |> String.split_on_char ' '
    |> List.filter (fun x -> String.length x > 0)
  with
  | [ h ] -> variant_match h
  | _ -> raise InvalidVariant

let response_match x =
  if x = "Y" then Yes else if x = "N" then No else raise InvalidResponse

let parse_draw_offer str =
  match
    str |> String.split_on_char ' '
    |> List.filter (fun x -> String.length x > 0)
  with
  | [ h ] -> response_match h
  | _ -> raise InvalidResponse
