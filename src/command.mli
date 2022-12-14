(** Parsing user input into moves and actions *)

exception InvalidPiece
(** Raised when a user attempts to make a move, but doesn't input a valid piece
    to do it with. *)

exception InvalidSquare
(** Raised when a user attempts to make a move and inputs a valid piece, but
    then inputs an invalid square.*)

exception InvalidCommand
(** Raised when user input cannot be meaningfully parsed when taking an action. *)

exception InvalidVariant
(** Raised when user doesn't input a valid variant to play at the beginning. *)

exception InvalidResponse
(** Raised when user doesn't input a valid response to a draw offer from his
    opponent.*)

(** type [command] represents a player command decomposed into either an
    attempt at a move, an indication to resign, or a draw offer.*)
type command =
  | Move of Board.piece_type * string list
  | Resign
  | DrawOffer
      
 (** type [variant] represents the variant selected by the players at the
    start of the game: one of Standard, 3-check, KOTH, or Fischer Random.*)
type variant =
  | Standard
  | ThreeCheck
  | KingOfTheHill
  | FischerRandom
     
(** type [rounds] represents a best-of match between the players.*)
type rounds =
  | BestOf of int
      
(** Represents the response to a draw offer. *)
type response =
  | Yes
  | No  


val string_of_variant : variant -> string
(** Returns a string representation of a variant.*)

val string_of_rounds : rounds -> string
(** Represents a string representation of the number of rounds played. *)

val parse_move : string list -> command
(** Parses a player input into a command. *)

val parse : string -> Board.piece_color -> Board.state -> command
(** Parses a player input into a command. Requires: the input must contain only
    alphanumeric characters or space characters *)

val parse_promotion : string -> Board.piece_type
(** Parses the desired piece type upon promotion of a pawn.*)

val parse_variant : string -> variant
(** Parses the selection of a variant at the beginning of a game.*)

val parse_rounds : string -> rounds
(** Parses the selection of the number of rounds selected.*)

val parse_response : string -> response
(** Parses the response to a draw offer or a takeback.*)