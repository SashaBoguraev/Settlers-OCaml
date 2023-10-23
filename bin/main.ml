open Game

let player_one = Game.Player.Player.empty
let player_two = Game.Player.Player.empty
let initial_board = Game.Board.SmallBoard.initial_board

type state = {player1: Game.Player.Player.t; player2: Game.Player.Player.t; board: Game.Board.SmallBoard.t}

(* read-eval-print loop for fourth piece placement
   if valid input: place piece, exit loop and print setup output 
   if invalid input: rerun loop*)
let rec repl_3 (cur_state: state): unit = 
  print_endline "Player One Second Settlement > ";
  let input = int_of_string(read_line ()) in
  if (input < 1) then repl_3  cur_state
  else if (input > 24) then repl_3  cur_state
  else match Game.Board.SmallBoard.build_settlement cur_state.board input, Game.Player.Player.build_settlment cur_state.player1 input with
  | Some a, (Some b, _) -> print_endline "SHABOOYA"
  | None, _ -> (print_endline "There is already a piece at this settlment location"; repl_3 cur_state)
  | _ -> (print_string "UNEXPECTED BEHAVIOR TURN BACK NOW YOUR LIFE IS IN GREAT DANGER")

(* read-eval-print loop for third piece placement
   if valid input: place piece, exit loop by calling repl_3 
   if invalid input: rerun loop*)
let rec repl_2 (cur_state: state) : unit = 
  print_endline "Player Two Second Settlement > ";
  let input = int_of_string(read_line ()) in
  if (input < 1) then repl_2  cur_state
  else if (input > 24) then repl_2  cur_state
  else match Game.Board.SmallBoard.build_settlement cur_state.board input, Game.Player.Player.build_settlment cur_state.player2 input with
  | Some a, (Some b, _) -> repl_3 {cur_state with player2 = b; board = a}
  | None, _ -> (print_endline "There is already a piece at this settlment location"; repl_2 cur_state)
  | _ -> (print_string "UNEXPECTED BEHAVIOR TURN BACK NOW YOUR LIFE IS IN GREAT DANGER")

(* read-eval-print loop for second piece placement
   if valid input: place piece, exit loop by calling repl_2 
   if invalid input: rerun loop*)
let rec repl_1 (cur_state: state): unit =
  print_endline "Player Two First Settlement > ";
  let input = int_of_string(read_line ()) in
  if (input < 1) then repl_1  cur_state
  else if (input > 24) then repl_1  cur_state
  else match Game.Board.SmallBoard.build_settlement cur_state.board input, Game.Player.Player.build_settlment cur_state.player2 input with
  | Some a, (Some b, _) -> repl_2 {cur_state with player2 = b; board = a}
  | None, _ -> (print_endline "There is already a piece at this settlment location"; repl_1 cur_state)
  | _ -> (print_string "UNEXPECTED BEHAVIOR TURN BACK NOW YOUR LIFE IS IN GREAT DANGER") 

let () =
  print_endline "\n\nWelcome to Catan!\n";
  print_endline "           / \\     / \\";
  print_endline "          /   \\   /   \\";
  print_endline "         /     \\ /     \\";
  print_endline "        | Sheep | Wood  | ";
  print_endline "        |   6   |   4   | ";
  print_endline "       / \\     / \\     / \\";
  print_endline "      /   \\   /   \\   /   \\";
  print_endline "     /     \\ /     \\ /     \\";
  print_endline "    | Clay  | Desert|  Wood |";
  print_endline "    |   3   |       |   2   |";
  print_endline "    |       |       |       |";
  print_endline "     \\     / \\     / \\     /";
  print_endline "      \\   /   \\   /   \\   /";
  print_endline "       \\ /     \\ /     \\ /";
  print_endline "        | Wheat |  Ore  | ";
  print_endline "        |   5   |   1   | ";
  print_endline "         \\     / \\     / ";
  print_endline "          \\   /   \\   / ";
  print_endline "           \\ /     \\ / \n";
  print_endline "Possible locations are labeled 1-24 starting top left and increase right then down\n";
  print_endline "Player 1 please enter the number of the node for your first settlement: ";
  let position = int_of_string (read_line ()) in 
  match Game.Board.SmallBoard.build_settlement initial_board position, Game.Player.Player.build_settlment player_one position with
  | Some a, (None, _) -> print_string "LEO FUCKED UP"
  | None, (Some a, _) -> print_string "SASHA FUCKED UP"
  | Some a, (Some b, _) -> repl_1({player1 = b; player2 = player_two; board = a})
  | _ -> print_endline "SOMETHING VERY WRONG; ABORT; RETRY; DESTROY"
  ;











