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

type rounds = BestOf of int

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

let rounds_match x =
  if x = "Single" then BestOf 1
  else if String.starts_with "Best of " x then
    let substr = String.sub x 8 (String.length x - 8) in
    try BestOf (int_of_string substr) with _ -> raise InvalidVariant
  else raise InvalidVariant

let string_of_variant = function
  | Standard -> "Standard"
  | ThreeCheck -> "3-check"
  | KingOfTheHill -> "King of the Hill"

let string_of_rounds = function
  | BestOf i -> if i = 1 then "Single" else "Best of " ^ string_of_int i

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
  | [ "Best"; "of"; s ] -> variant_match ("Best of " ^ s)
  | _ -> raise InvalidVariant

let parse_rounds str =
  match
    str |> String.split_on_char ' '
    |> List.filter (fun x -> String.length x > 0)
  with
  | [ h ] -> rounds_match h
  | [ "Best"; "of"; s ] -> rounds_match ("Best of " ^ s)
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
