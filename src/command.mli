(** Interpreting string input into useful formats *)

exception InvalidPiece
exception InvalidSquare
exception InvalidCommand
exception InvalidVariant
exception InvalidResponse

type command =
  | Move of Board.piece_type * string list
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

val piece_match : string -> Board.piece_type
val variant_match : string -> variant
val rounds_match : string -> rounds
val string_of_variant : variant -> string
val string_of_rounds : rounds -> string
val parse : string -> command
val parse_promotion : string -> Board.piece_type
val parse_variant : string -> variant
val parse_rounds : string -> rounds
val parse_draw_offer : string -> response