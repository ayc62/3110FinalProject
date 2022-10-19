open Stdlib
open Game
open Board
open Check
open Command
open Controller
open Printboard

(** [main ()] prompts for the game to play, then starts it. *)

let rec execute_player_move (color : piece_color) (state : state) (cmd : string)
    =
  try
    match parse cmd with
    | Move (piece, moves) -> print_endline "didnt finish this yet"
    | Print -> Printboard.print_board state
    | Resign -> print_endline "u resigned L"
  with Command.InvalidCommand ->
    print_endline "This is not a valid command as entered. Please try again."

and get_new_player_move = ()

let main () =
  print_endline "Welcome to a very unfinished implementation of Chess.";
  print_endline
    "Rules: You may make a move by entering 'move [piece name] [starting \
     square] [ending square]', such as 'move Pawn e2 e4'. You may also \
     visualize the board by typing 'print', or surrender the game by typing \
     'resign'.";
  print_endline "To move: White";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> execute_player_move White init_state command

(** Execute the game engine. *)

let () = main ()