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
let games_played = ref 0
let cur_state = ref init_state
let is_won = ref false

let gen_new_fischer should_change =
  if should_change then (
    Random.self_init ();
    let temp = fischer_random_state () in
    cur_state := temp;
    !cur_state)
  else !cur_state

let win_print_score color =
  is_won := true;
  if !white_score > !black_score then
    if Float.is_integer !white_score then
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        ("White wins "
        ^ (!white_score |> int_of_float |> string_of_int)
        ^ "-"
        ^ (!black_score |> int_of_float |> string_of_int)
        ^ "! Thanks for playing.")
    else
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        ("White wins "
        ^ string_of_float !white_score
        ^ "-"
        ^ string_of_float !black_score
        ^ "! Thanks for playing!")
  else if !black_score > !white_score then
    if Float.is_integer !white_score then
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        ("Black wins "
        ^ (!black_score |> int_of_float |> string_of_int)
        ^ "-"
        ^ (!white_score |> int_of_float |> string_of_int)
        ^ "! Thanks for playing!")
    else
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        ("Black wins "
        ^ string_of_float !black_score
        ^ "-"
        ^ string_of_float !white_score
        ^ "! Thanks for playing!")
  else if Float.is_integer !white_score then
    ANSITerminal.print_string [ ANSITerminal.cyan ]
      ("Black wins "
      ^ (!black_score |> int_of_float |> string_of_int)
      ^ "-"
      ^ (!white_score |> int_of_float |> string_of_int)
      ^ "! Thanks for playing!")
  else if Float.is_integer !white_score then
    ANSITerminal.print_string [ ANSITerminal.cyan ]
      ("It's tied "
      ^ (!black_score |> int_of_float |> string_of_int)
      ^ "-"
      ^ (!white_score |> int_of_float |> string_of_int)
      ^ "! Thanks for playing!")
  else
    ANSITerminal.print_string [ ANSITerminal.cyan ]
      ("It's tied "
      ^ string_of_float !black_score
      ^ "-"
      ^ string_of_float !white_score
      ^ "! Thanks for playing!")

let continue_game_print_score color =
  if !white_score > !black_score then
    if Float.is_integer !white_score then
      print_endline
        ((color |> color_string) ^ " wins! White leads "
        ^ (!white_score |> int_of_float |> string_of_int)
        ^ "-"
        ^ (!black_score |> int_of_float |> string_of_int)
        ^ "\n")
    else
      print_endline
        ((color |> color_string) ^ " wins! White leads "
        ^ string_of_float !white_score
        ^ "-"
        ^ string_of_float !black_score
        ^ "\n")
  else if !white_score < !black_score then
    if Float.is_integer !black_score then
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        ((color |> color_string) ^ " wins! Black leads "
        ^ (!black_score |> int_of_float |> string_of_int)
        ^ "-"
        ^ (!white_score |> int_of_float |> string_of_int)
        ^ ".\n")
    else
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        ((color |> color_string) ^ " wins! Black leads "
        ^ string_of_float !black_score
        ^ "-"
        ^ string_of_float !white_score
        ^ ".\n")
  else if Float.is_integer !black_score then
    ANSITerminal.print_string [ ANSITerminal.cyan ]
      ((color |> color_string) ^ " wins! The game is tied "
      ^ (!black_score |> int_of_float |> string_of_int)
      ^ "-"
      ^ (!white_score |> int_of_float |> string_of_int)
      ^ ".\n")
  else
    ANSITerminal.print_string [ ANSITerminal.cyan ]
      ((color |> color_string) ^ " wins! The game is tied "
      ^ string_of_float !black_score
      ^ "-"
      ^ string_of_float !white_score
      ^ ".\n")

let print_score color =
  if
    !games_played = !num_games
    || !white_score >= (float_of_int !num_games /. 2.) +. 0.5
    || !black_score >= (float_of_int !num_games /. 2.) +. 0.5
  then win_print_score color
  else continue_game_print_score color

let is_koth_won color state =
  square_has_pt state "d4" King color
  || square_has_pt state "d5" King color
  || square_has_pt state "e4" King color
  || square_has_pt state "e5" King color

let rec execute_player_move (color : piece_color) (state : state) (cmd : string)
    =
  try
    match parse cmd color state with
    | Move (piece, moves) ->
        let result_of_move =
          Controller.move_piece piece color (List.hd moves)
            (moves |> List.rev |> List.hd)
            state
        in
        match_result result_of_move color state
    | DrawOffer -> get_draw color state
    | Resign ->
        games_played := !games_played + 1;
        if !num_games = 1 then
          ANSITerminal.print_string [ ANSITerminal.cyan ]
            ((color |> opposite_color |> color_string)
            ^ " wins! Thanks for playing! \n")
        else (
          if color = White then black_score := !black_score +. 1.
          else white_score := !white_score +. 1.;
          print_score (opposite_color color);
          if not !is_won then
            let new_state = gen_new_fischer true in
            if !variant_selected = FischerRandom then
              get_new_player_move White new_state
            else get_new_player_move White init_state)
  with
  | Command.InvalidCommand ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "This is not a valid command as entered. Please try again.";
      print_endline "";
      get_new_player_move ~print:false color state
  | Command.InvalidSquare ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "One of the squares specified is invalid. Please try again.";
      print_endline "";
      get_new_player_move ~print:false color state
  | Command.InvalidPiece ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "The piece specified is invalid. Please try again.";
      print_endline "";
      get_new_player_move ~print:false color state

and match_result (result : result) (color : piece_color) (state : state) =
  match result with
  | Legal st ->
      if is_koth_won color st then
        if !num_games = 1 then
          ANSITerminal.print_string [ ANSITerminal.cyan ]
            ((color |> color_string) ^ " wins! Thanks for playing!")
        else (
          if color = White then white_score := !white_score +. 1.
          else black_score := !black_score +. 1.;
          print_score color;
          if not !is_won then get_new_player_move White init_state);
      get_new_player_move (opposite_color color) st
  | Check st ->
      if !variant_selected = ThreeCheck then
        if color = White then begin
          check_counter_white := !check_counter_white + 1;
          if !check_counter_white = 3 then (
            if !num_games = 1 then
              ANSITerminal.print_string [ ANSITerminal.cyan ]
                "White wins! Thanks for playing!\n"
            else print_score color;
            if not !is_won then get_new_player_move White init_state)
        end
        else begin
          check_counter_black := !check_counter_black + 1;
          if !check_counter_black = 3 then (
            if !num_games = 1 then
              ANSITerminal.print_string [ ANSITerminal.cyan ]
                "White wins! Thanks for playing!\n"
            else if color = White then white_score := !white_score +. 1.
            else black_score := !black_score +. 1.;
            print_score color;
            if not !is_won then get_new_player_move White init_state)
        end;
      if !variant_selected = KingOfTheHill then
        if is_koth_won color st then
          if !num_games = 1 then
            ANSITerminal.print_string [ ANSITerminal.cyan ]
              ((color |> color_string) ^ " wins! Thanks for playing!\n")
          else (
            if color = White then white_score := !white_score +. 1.
            else black_score := !black_score +. 1.;
            print_score color;
            if not !is_won then get_new_player_move White init_state);
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        ((color |> opposite_color |> color_string) ^ " is under check!");
      print_endline "";
      get_new_player_move (opposite_color color) st
  | Draw st | Stalemate st ->
      if !variant_selected = KingOfTheHill then
        if is_koth_won color st then
          if !num_games = 1 then
            ANSITerminal.print_string [ ANSITerminal.cyan ]
              ((color |> color_string) ^ " wins! Thanks for playing!\n")
          else (
            if color = White then white_score := !white_score +. 1.
            else black_score := !black_score +. 1.;
            print_score color;
            if not !is_won then get_new_player_move White init_state)
        else (
          white_score := !white_score +. 0.5;
          black_score := !black_score +. 0.5;
          print_score color)
      else if !num_games = 1 then
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          "It is a draw! Thanks for playing!\n"
      else (
        white_score := !white_score +. 0.5;
        black_score := !black_score +. 0.5;
        print_score color;
        if not !is_won then
          let new_state = gen_new_fischer true in
          if !variant_selected = FischerRandom then
            get_new_player_move White new_state
          else get_new_player_move White init_state)
  | Checkmate st ->
      games_played := !games_played + 1;
      if !num_games = 1 then
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          ((color |> color_string) ^ " wins! Thanks for playing!\n")
      else (
        if color = White then white_score := !white_score +. 1.
        else black_score := !black_score +. 1.;
        print_score color;
        if not !is_won then
          let new_state = gen_new_fischer true in
          if !variant_selected = FischerRandom then
            get_new_player_move White new_state
          else get_new_player_move White init_state)
  | PawnPromotion st ->
      match_result
        (get_promoted_piece color (fst (List.hd st.board)) st)
        color state
  | Illegal ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "The specified move is illegal. Please try again.";
      print_endline "";
      get_new_player_move ~print:false color state

and get_new_player_move ?(print = true) color (state : state) =
  if print then
    if color = White then
      Printboard.print_board_white state !variant_selected (BestOf !num_games)
    else
      Printboard.print_board_black state !variant_selected (BestOf !num_games)
  else ();
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    ("To move: " ^ (color |> color_string));
  print_endline "";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> execute_player_move color state command

and get_promoted_piece color (pos : string) (state : state) : result =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "What piece would you like to promote to?";
  print_endline "";
  print_string "> ";
  match read_line () with
  | piece -> begin
      try promote_pawn color pos (parse_promotion piece) state
      with InvalidPiece ->
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          "Invalid piece, please try again.";
        print_endline "";
        get_promoted_piece color pos state
    end

and get_draw color state =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    ((color |> color_string) ^ " has offered a draw. "
    ^ (color |> opposite_color |> color_string)
    ^ ", would you like to accept? Type Y or N to respond.");
  print_endline "";
  print_endline "> ";
  match read_line () with
  | response -> begin
      try
        match parse_response response with
        | Yes ->
            games_played := !games_played + 1;
            if !num_games = 1 then
              ANSITerminal.print_string [ ANSITerminal.cyan ]
                "It is a draw! Thanks for playing!"
            else (
              white_score := !white_score +. 0.5;
              black_score := !black_score +. 0.5;
              print_score color;
              if not !is_won then
                let new_state = gen_new_fischer true in
                if !variant_selected = FischerRandom then
                  get_new_player_move White new_state
                else get_new_player_move White init_state)
        | No -> get_new_player_move color state
      with InvalidResponse ->
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          "Invalid response, please try again.";
        print_endline "";
        get_draw color state
    end

let rec get_variant () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Select a variant by typing 'Standard', '3-check', 'KOTH', or 'Fischer \
     Random'. All options are case-sensitive.";
  print_endline "";
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
        | FischerRandom -> variant_selected := FischerRandom
      with InvalidVariant ->
        ANSITerminal.print_string [ ANSITerminal.green ]
          "Invalid variant selected. Please try again.";
        print_endline "";
        get_variant ())

let rec get_rounds () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "How many rounds of the game would you like to play? Type 'Single' for 1 \
     round, or 'Best of [int]' for multiple rounds. All options are \
     case-sensitive.";
  print_endline "";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | str -> (
      try
        let rnds = parse_rounds str in
        match rnds with
        | BestOf i -> if i <= 0 then raise InvalidVariant else num_games := i
      with InvalidVariant ->
        print_endline "Invalid variant selected. Please try again.";
        get_rounds ())

let rec main () =
  white_score := 0.;
  black_score := 0.;
  games_played := 0;
  is_won := false;
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Welcome to our CS 3110 Final Project, an implementation of chess in OCaml!";
  print_endline "";
  print_endline
    "We currently support standard chess, 3-check, King of the Hill, and \
     Fischer Random/Chess960. For a complete description of the rules of each \
     of these variants, or even just to refresh the rules of standard chess, \
     please visit the README on our GitHub repository, which contains helpful \
     links to rules for every variant.";
  get_variant ();
  get_rounds ();
  print_endline
    "To make a move on the chessboard, enter 'move [piece name] [starting \
     square] [ending square]', such as 'move Pawn e2 e4'.";
  print_endline
    "To castle either kingside or queenside, type 'castle kingside' or 'castle \
     queenside'.";
  print_endline
    "To resign the game, type'resign', or offer a draw by typing 'draw', which \
     will prompt for a response from the other player.";
  print_endline
    "Finally, entering [ctrl-c] at any point will abort the program.";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Note that all keywords are case-sensitive!";
  print_endline "";
  let new_state = gen_new_fischer true in
  if !variant_selected = FischerRandom then
    Printboard.print_board_white new_state !variant_selected (BestOf !num_games)
  else
    Printboard.print_board_white init_state !variant_selected
      (BestOf !num_games);
  ANSITerminal.print_string [ ANSITerminal.cyan ] "To move: White";
  print_endline "";
  print_string "> ";
  begin
    match read_line () with
    | exception End_of_file -> ()
    | command ->
        if !variant_selected = FischerRandom then
          execute_player_move White new_state command
        else execute_player_move White init_state command
  end;
  print_endline "";
  restart_game ()

and restart_game () =
  white_score := 0.;
  black_score := 0.;
  games_played := 0;
  is_won := false;
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Would you like to play again? Type Y or N to respond.";
  print_endline "";
  print_endline "> ";
  match read_line () with
  | response -> begin
      try
        match parse_response response with
        | Yes -> main ()
        | No ->
            ANSITerminal.print_string [ ANSITerminal.cyan ]
              "Thanks for playing, all the same!"
      with InvalidResponse ->
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          "Invalid response, please try again.";
        restart_game ()
    end

(** Execute the game engine. *)

let () = main ()
