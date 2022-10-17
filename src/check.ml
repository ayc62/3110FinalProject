open Board

exception OccupiedSquare of string
exception InvalidMove of string

let rec check_horizontal orig_pos new_pos state =
  let diff =
    Char.code (String.get new_pos 0) - Char.code (String.get orig_pos 0)
  in
  if abs diff = 1 then true
  else
    let next_pos =
      String.get orig_pos 0 |> Char.code
      |> ( + ) (diff / abs diff)
      |> Char.chr |> String.make 1
      |> ( ^ ) (String.get orig_pos 1 |> Char.escaped)
    in
    if List.assoc_opt next_pos state = None then
      check_horizontal next_pos new_pos state
    else false

let rec check_vertical orig_pos new_pos state =
  let diff =
    Char.code (String.get new_pos 1) - Char.code (String.get orig_pos 1)
  in
  if abs diff = 1 then true
  else
    let next_pos =
      String.get orig_pos 0 |> Char.escaped
      |> ( ^ )
           (String.get orig_pos 1 |> Char.code
           |> ( + ) (diff / abs diff)
           |> Char.chr |> String.make 1)
    in
    if List.assoc_opt next_pos state = None then
      check_horizontal next_pos new_pos state
    else false

let rec check_diagonal orig_pos new_pos state = true

let check_pawn color orig_pos new_pos state =
  let piece_state : piece = List.assoc orig_pos state in
  if String.get orig_pos 0 = String.get new_pos 0 then
    let dir = if color = White then 1 else -1 in
    if
      Char.code (String.get new_pos 1) - Char.code (String.get orig_pos 1) = dir
      && List.assoc_opt new_pos state = None
    then true
    else if
      piece_state.moved
      && check_vertical orig_pos new_pos state
      && List.assoc_opt new_pos state = None
    then true
    else false
  else false
