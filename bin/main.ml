open Stdlib
open Game
open Board
open Check
open Command
open Controller
open Printboard

(** [main ()] prompts for the game to play, then starts it. *)
let opposite_color (color : piece_color) =
  match color with
  | White -> Black
  | Black -> White

let color_string (color : piece_color) =
  match color with
  | White -> "White"
  | Black -> "Black"

let rec execute_player_move (color : piece_color) (state : state) (cmd : string)
    =
  try
    match parse cmd with
    | Move (piece, moves) -> (
        let attempted_state =
          Controller.move_piece piece color (List.hd moves)
            (moves |> List.rev |> List.hd)
            state
        in
        match attempted_state with
        | Legal st -> get_new_player_move (opposite_color color) st
        | Check st ->
            print_endline "You are under check";
            get_new_player_move (opposite_color color) st
        | Draw st | Stalemate st ->
            print_endline "It is a draw! Thank you for playing."
        | Checkmate st ->
            print_endline
              ((color |> color_string) ^ " wins! Thank you for playing.")
        | PawnPromotion st ->
            let st = promote_pawn color "h8" Queen st in
            get_new_player_move (opposite_color color) st
        | Illegal ->
            print_endline "The specified move is illegal. Please try again.";
            get_new_player_move ~print:false color state)
    | Resign ->
        print_endline
          ((color |> opposite_color |> color_string)
          ^ " wins! Thank you for playing.")
  with
  | Command.InvalidCommand ->
      print_endline "This is not a valid command as entered. Please try again.";
      get_new_player_move ~print:false color state
  | Command.InvalidSquare ->
      print_endline "One of the squares specified is invalid. Please try again.";
      get_new_player_move ~print:false color state
  | Command.InvalidPiece ->
      print_endline "The piece specified is invalid. Please try again.";
      get_new_player_move ~print:false color state

and get_new_player_move ?(print = true) color (state : state) =
  if print then
    if color = White then Printboard.print_board_white state
    else Printboard.print_board_black state
  else ();
  print_endline ("To move: " ^ (color |> color_string));
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> execute_player_move color state command

let main () =
  print_endline "Welcome to a very unfinished implementation of Chess.";
  print_endline
    "So far, we only have the standard version working, with the caveat that \
     castling and checkmate have not been implemented.";
  print_endline
    "A player may make a move by entering 'move [piece name] [starting square] \
     [ending square]', such as 'move Pawn e2 e4'. Note that the piece names \
     and squares are case-sensitive: the piece name should be capitalized, and \
     the squares should not be capitalized. You can also resign the game by \
     typing 'resign'.";
  Printboard.print_board_white init_state;
  print_endline "To move: White";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> execute_player_move White init_state command

(** Execute the game engine. *)

let () = main ()