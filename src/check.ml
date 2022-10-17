open Board

exception OccupiedSquare of string
exception InvalidMove of string

(** [check_horizontal orig_pos new_pos state] checks if there are any piecees
    between [orig_pos] and [new_pos] in the board [state]. Requires: [orig_pos]
    and [new_pos] are horizontal from each other and [state] is a valid state of
    the board *)
let rec check_horizontal orig_pos new_pos state =
  let diff =
    Char.code (String.get new_pos 0) - Char.code (String.get orig_pos 0)
  in
  if abs diff = 1 then true
  else
    let next_pos =
      String.get orig_pos 1 |> Char.escaped
      |> ( ^ )
           (String.get orig_pos 0 |> Char.code
           |> ( + ) (diff / abs diff)
           |> Char.chr |> String.make 1)
    in
    if List.assoc_opt next_pos state = None then
      check_horizontal next_pos new_pos state
    else false

(** [check_vertical orig_pos new_pos state] checks if there are any piecees
    between [orig_pos] and [new_pos] in the board [state]. Requires: [orig_pos]
    and [new_pos] are vertical from each other and [state] is a valid state of
    the board *)
let rec check_vertical orig_pos new_pos state =
  let diff =
    Char.code (String.get new_pos 1) - Char.code (String.get orig_pos 1)
  in
  if abs diff = 1 then true
  else
    let next_pos =
      String.get orig_pos 1 |> Char.code
      |> ( + ) (diff / abs diff)
      |> Char.chr |> String.make 1
      |> ( ^ ) (String.get orig_pos 0 |> Char.escaped)
    in
    if List.assoc_opt next_pos state = None then
      check_vertical next_pos new_pos state
    else false

(** [check_diagonal orig_pos new_pos state] checks if there are any piecees
    between [orig_pos] and [new_pos] in the board [state]. Requires: [orig_pos]
    and [new_pos] are diagonal from each other and [state] is a valid state of
    the board *)
let rec check_diagonal orig_pos new_pos state = true

(** [check_pawn color orig_pos new_pos state] checks if moving a pawn from
    [orig_pos] to [new_pos] is a valid move. Requires: [orig_pos] and [new_pos]
    are valid squares and [state] is a valid state of the board *)
let check_pawn color orig_pos new_pos state =
  let piece_state : piece = List.assoc orig_pos state in
  if String.get orig_pos 0 = String.get new_pos 0 then
    let dir = if color = White then 1 else -1 in
    if
      Char.code (String.get new_pos 1) - Char.code (String.get orig_pos 1) = dir
      && List.assoc_opt new_pos state = None
    then true
    else if
      (not piece_state.moved)
      && check_vertical orig_pos new_pos state
      && List.assoc_opt new_pos state = None
    then true
    else false
  else (*TODO: Logic for caputuring pieces*) false
