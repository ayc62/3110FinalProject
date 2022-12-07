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

let variant_selected = ref Standard
let check_counter_white = ref 0
let check_counter_black = ref 0
let white_score = ref 0.
let black_score = ref 0.
let num_games = ref 1

let print_score color =
  if !num_games = 0 then
    if !white_score > !black_score then
      if Float.is_integer !white_score then (
        print_endline
          ("White wins the game "
          ^ (!white_score |> int_of_float |> string_of_int)
          ^ "-"
          ^ (!black_score |> int_of_float |> string_of_int)
          ^ "! Thanks for playing.");
        exit 0)
      else (
        print_endline
          ("White wins the game "
          ^ string_of_float !white_score
          ^ "-"
          ^ string_of_float !black_score
          ^ "! Thanks for playing.");
        exit 0)
    else if !black_score > !white_score then
      if Float.is_integer !white_score then (
        print_endline
          ("Black wins the game "
          ^ (!black_score |> int_of_float |> string_of_int)
          ^ "-"
          ^ (!white_score |> int_of_float |> string_of_int)
          ^ "! Thanks for playing.");
        exit 0)
      else (
        print_endline
          ("Black wins the game "
          ^ string_of_float !black_score
          ^ "-"
          ^ string_of_float !white_score
          ^ "! Thanks for playing.");
        exit 0)
    else if Float.is_integer !white_score then (
      print_endline
        ("Black wins the game "
        ^ (!black_score |> int_of_float |> string_of_int)
        ^ "-"
        ^ (!white_score |> int_of_float |> string_of_int)
        ^ "! Thanks for playing.");
      exit 0)
    else if Float.is_integer !white_score then (
      print_endline
        ("The game is tied "
        ^ (!black_score |> int_of_float |> string_of_int)
        ^ "-"
        ^ (!white_score |> int_of_float |> string_of_int)
        ^ "! Thanks for playing.");
      exit 0)
    else (
      print_endline
        ("The game is tied "
        ^ string_of_float !black_score
        ^ "-"
        ^ string_of_float !white_score
        ^ "! Thanks for playing.");
      exit 0)
  else if !white_score > !black_score then
    if Float.is_integer !white_score then
      print_endline
        ((color |> color_string) ^ " wins! White leads "
        ^ (!white_score |> int_of_float |> string_of_int)
        ^ "-"
        ^ (!black_score |> int_of_float |> string_of_int))
    else
      print_endline
        ((color |> color_string) ^ " wins! White leads "
        ^ string_of_float !white_score
        ^ "-"
        ^ string_of_float !black_score)
  else if !white_score < !black_score then
    if Float.is_integer !black_score then
      print_endline
        ((color |> color_string) ^ " wins! Black leads "
        ^ (!black_score |> int_of_float |> string_of_int)
        ^ "-"
        ^ (!white_score |> int_of_float |> string_of_int))
    else
      print_endline
        ((color |> color_string) ^ " wins! Black leads "
        ^ string_of_float !black_score
        ^ "-"
        ^ string_of_float !white_score)
  else if Float.is_integer !black_score then
    print_endline
      ((color |> color_string) ^ " wins! The game is tied "
      ^ (!black_score |> int_of_float |> string_of_int)
      ^ "-"
      ^ (!white_score |> int_of_float |> string_of_int))
  else
    print_endline
      ((color |> color_string) ^ " wins! The game is tied "
      ^ string_of_float !black_score
      ^ "-"
      ^ string_of_float !white_score)

let is_koth_won color state =
  square_has_pt state "d4" King color
  || square_has_pt state "d5" King color
  || square_has_pt state "e4" King color
  || square_has_pt state "e5" King color

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
        | Legal st ->
            if !variant_selected = KingOfTheHill then
              if is_koth_won color st then (
                print_endline
                  ((color |> color_string) ^ " wins! Thank you for playing.");
                exit 0);
            get_new_player_move (opposite_color color) st
        | Check st ->
            if !variant_selected = ThreeCheck then
              if color = White then begin
                check_counter_white := !check_counter_white + 1;
                if !check_counter_white = 3 then (
                  print_endline "White wins! Thank you for playing.";
                  exit 0)
              end
              else begin
                check_counter_black := !check_counter_black + 1;
                if !check_counter_black = 3 then (
                  print_endline "Black wins! Thank you for playing.";
                  exit 0)
              end;
            if !variant_selected = KingOfTheHill then
              if is_koth_won color st then (
                print_endline
                  ((color |> color_string) ^ " wins! Thank you for playing.");
                exit 0);
            print_endline
              ((color |> opposite_color |> color_string) ^ " is under check!");
            get_new_player_move (opposite_color color) st
        | Draw st | Stalemate st ->
            if !variant_selected = KingOfTheHill then
              if is_koth_won color st then (
                print_endline
                  ((color |> color_string) ^ " wins! Thank you for playing.");
                exit 0);
            print_endline "It is a draw! Thank you for playing."
        | Checkmate st -> begin
            match !variant_selected with
            | BestOf i ->
                num_games := !num_games - 1;
                if color = White then white_score := !white_score +. 1.
                else black_score := !black_score +. 1.;
                print_score color;
                get_new_player_move color init_state
            | _ ->
                print_endline
                  ((color |> color_string) ^ " wins! Thank you for playing.")
          end
        | PawnPromotion st ->
            get_new_player_move (opposite_color color)
              (get_promoted_piece color (moves |> List.rev |> List.hd) st)
        | Illegal ->
            print_endline "The specified move is illegal. Please try again.";
            get_new_player_move ~print:false color state)
    | DrawOffer -> get_draw color state
    | Resign -> begin
        match !variant_selected with
        | BestOf i ->
            num_games := !num_games - 1;
            if color = White then black_score := !black_score +. 1.
            else white_score := !white_score +. 1.;
            print_score color
        | _ ->
            print_endline
              ((color |> opposite_color |> color_string)
              ^ " wins! Thank you for playing.")
      end
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
    if color = White then Printboard.print_board_white state !variant_selected
    else Printboard.print_board_black state !variant_selected
  else ();
  print_endline ("To move: " ^ (color |> color_string));
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> execute_player_move color state command

and get_promoted_piece color pos (state : state) : state =
  print_endline "What piece would you like to promote to?";
  print_string "> ";
  match read_line () with
  | piece -> begin
      try promote_pawn color pos (parse_promotion piece) state
      with InvalidPiece ->
        print_endline "Invalid piece, please try again.";
        get_promoted_piece color pos state
    end

and get_draw color state =
  print_endline
    ((color |> color_string) ^ " has offered a draw. "
    ^ (color |> opposite_color |> color_string)
    ^ ", would you like to accept? Type Y or N to respond.");
  print_endline "> ";
  match read_line () with
  | response -> begin
      try
        match parse_draw_offer response with
        | Yes -> print_endline "It is a draw! Thank you for playing."
        | No -> get_new_player_move color state
      with InvalidResponse ->
        print_endline "Invalid response, please try again.";
        get_draw color state
    end

let rec get_variant () =
  print_endline
    "Select an option by typing 'Standard', '3-check', 'KOTH', or 'Best of \
     [int]'. All options are case-sensitive.";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | str -> (
      try
        let var = parse_variant str in
        match var with
        | Standard -> ()
        | ThreeCheck -> variant_selected := ThreeCheck
        | KingOfTheHill -> variant_selected := KingOfTheHill
        | BestOf i ->
            variant_selected := BestOf i;
            num_games := i
      with InvalidVariant ->
        print_endline "Invalid variant selected. Please try again.";
        get_variant ())

let main () =
  print_endline "Welcome to a very unfinished implementation of Chess.";
  print_endline
    "We currently support standard chess, 3-check, and king of the hill; \
     3-check operates on the same rules as standard chess but the game ends \
     immediately after one player checks the opponent 3 times, and king of the \
     hill ends immediately after one of the kings reaches the center four \
     squares: d4, e4, d5, or e5.";
  get_variant ();
  print_endline
    "A player may make a move by entering 'move [piece name] [starting square] \
     [ending square]', such as 'move Pawn e2 e4'. Note that the piece names \
     and squares are case-sensitive: the piece name should be capitalized, and \
     the squares should not be capitalized. You can also resign the game by \
     typing 'resign', or offer a draw by typing 'draw'.";
  Printboard.print_board_white init_state !variant_selected;
  print_endline "To move: White";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> execute_player_move White init_state command

(** Execute the game engine. *)

let () = main ()