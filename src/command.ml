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
  | FischerRandom

type rounds = BestOf of int

type response =
  | Yes
  | No

let piece_match x =
  if x = "pawn" then Pawn
  else if x = "knight" then Knight
  else if x = "bishop" then Bishop
  else if x = "queen" then Queen
  else if x = "king" then King
  else if x = "rook" then Rook
  else raise InvalidPiece

let variant_match x =
  if x = "Standard" then Standard
  else if x = "3-check" then ThreeCheck
  else if x = "KOTH" then KingOfTheHill
  else if x = "Fischer Random" then FischerRandom
  else raise InvalidVariant

let rounds_match x =
  if x = "single" then BestOf 1
  else if String.starts_with "best of " x then
    let substr = String.sub x 8 (String.length x - 8) in
    try BestOf (int_of_string substr) with _ -> raise InvalidVariant
  else raise InvalidVariant

let string_of_variant = function
  | Standard -> "Standard"
  | ThreeCheck -> "3-check"
  | KingOfTheHill -> "King of the Hill"
  | FischerRandom -> "Fischer Random"

let string_of_rounds = function
  | BestOf i -> if i = 1 then "single" else "best of " ^ string_of_int i

let castled_square (color : piece_color) (is_kingside : bool) : string =
  match color with
  | White -> if is_kingside then "g1" else "c1"
  | Black -> if is_kingside then "g8" else "c8"

let parse_move (move_cmd : string list) =
  match move_cmd with
  | [] | [ _ ] | [ _; _ ] -> raise InvalidCommand
  | [ h1; h2; h3 ] ->
      let piece = piece_match h1 in
      if (not (check_square h2)) || not (check_square h3) then
        raise InvalidSquare
      else Move (piece, [ h2; h3 ])
  | _ -> raise InvalidCommand

let parse str (color : piece_color) (state : state) =
  let parsed =
    str |> String.split_on_char ' '
    |> List.filter (fun x -> String.length x > 0)
    |> List.map String.lowercase_ascii
  in
  if parsed = [ "castle"; "kingside" ] then
    Move
      (King, [ king_square color state all_squares; castled_square color true ])
  else if parsed = [ "castle"; "queenside" ] then
    Move
      (King, [ king_square color state all_squares; castled_square color false ])
  else
    match parsed with
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
    |> List.map String.lowercase_ascii
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
  | [ "Fischer"; "Random" ] -> FischerRandom
  | _ -> raise InvalidVariant

let parse_rounds str =
  match
    str |> String.split_on_char ' '
    |> List.filter (fun x -> String.length x > 0)
    |> List.map String.lowercase_ascii
  with
  | [ h ] -> rounds_match h
  | [ "best"; "of"; s ] -> rounds_match ("best of " ^ s)
  | _ -> raise InvalidVariant

let response_match x =
  if x = "y" then Yes else if x = "n" then No else raise InvalidResponse

let parse_response str =
  match
    str |> String.split_on_char ' '
    |> List.filter (fun x -> String.length x > 0)
    |> List.map String.lowercase_ascii
  with
  | [ h ] -> response_match h
  | _ -> raise InvalidResponse