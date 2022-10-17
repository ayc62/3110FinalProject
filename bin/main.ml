open Stdlib

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string "> "

(** Execute the game engine. *)

let () = main ()