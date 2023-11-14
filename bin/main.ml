open Game

let player_one = Game.Player.Player.empty
let player_two = Game.Player.Player.empty
let initial_board = Game.Board.SmallBoard.initial_board

(*type state = {player1: Game.Player.Player.t; player2: Game.Player.Player.t; board: Game.Board.SmallBoard.t} *)

(* read-eval-print loop for a piece placement
   if valid input: place piece, exit loop and print setup output 
   if invalid input: rerun loop*)
let rec repl_piece (player : Game.Player.Player.t) (playernum: int) (piece : string)  (cur_state: state): (Game.Board.SmallBoard.t, Game.Player.Player.t) = 
  let s = "Player " ^ (string_of_int playernum) ^ "'s" ^ piece ^ "Settlement > " in
  print_endline s;
  let input = int_of_string(read_line ()) in
  if (input < 1) then repl_piece player piece cur_state
  else if (input > 24) then repl_piece player piece cur_state
  else (Game.Board.SmallBoard.build_settlement cur_state.board input, Game.Player.Player.build_settlment player input)
  |(* Some a, (Some b, _) -> print_endline "Board set up!"; 
                           print_string "Player one has settlements at: ";
                           print_string (string_of_int (input));
                           print_string ", ";
                           print_endline (string_of_int (pl_one_first));
                           print_endline "Player one has the following in hand: 3 settlements; 15 roads; 3 cities; 0 resources";
                           print_string "Player two has settlements at: ";
                           print_string (string_of_int (pl_two_first));
                           print_string ", ";
                           print_endline (string_of_int (pl_two_second));
                           print_endline "Player one has the following in hand: 3 settlements; 15 roads; 3 cities; 0 resources";

*)
                           
  | None, _ -> (print_endline "There is already a piece at this settlment location"; repl_3 pl_one_first pl_two_first pl_two_second cur_state)
  | _ -> (print_string "UNEXPECTED BEHAVIOR TURN BACK NOW YOUR LIFE IS IN GREAT DANGER")

(* read-eval-print loop for third piece placement
   if valid input: place piece, exit loop by calling repl_3 
   if invalid input: rerun loop*)
let rec repl_2 (pl_one_first : int) (pl_two_first : int) (board: Game.Board.SmallBoard.t) : unit = 
  print_endline "Player Two Second Settlement > ";
  let input = int_of_string(read_line ()) in
  if (input < 1) then repl_2 pl_one_first pl_two_first cur_state
  else if (input > 24) then repl_2 pl_one_first pl_two_first cur_state
  else match Game.Board.SmallBoard.build_settlement cur_state.board input, Game.Player.Player.build_settlment cur_state.player2 input with
  | Some a, (Some b, _) -> repl_3 pl_one_first pl_two_first input {cur_state with player2 = b; board = a}
  | None, _ -> (print_endline "There is already a piece at this settlment location"; repl_2 pl_one_first pl_two_first cur_state)
  | _ -> (print_string "UNEXPECTED BEHAVIOR TURN BACK NOW YOUR LIFE IS IN GREAT DANGER")

(* read-eval-print loop for second piece placement
   if valid input: place piece, exit loop by calling repl_2 
   if invalid input: rerun loop*)
let rec repl_1 (pl_one_first : int) (cur_state: state): unit =
  print_endline "Player Two First Settlement > ";
  let input = int_of_string(read_line ()) in
  if (input < 1) then repl_1 pl_one_first cur_state
  else if (input > 24) then repl_1 pl_one_first cur_state
  else match Game.Board.SmallBoard.build_settlement cur_state.board input, Game.Player.Player.build_settlment cur_state.player2 input with
  | Some a, (Some b, _) -> repl_2 pl_one_first input {cur_state with player2 = b; board = a}
  | None, _ -> (print_endline "There is already a piece at this settlment location"; repl_1 pl_one_first cur_state)
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
  let p1 = Game.Player.Player.empty in 
  let p2 = Game.Player.Player.empty in 
  let board = Game.Board.SmallBoard.initial_board
  let board, p1 = repl_piece p1 1 "first" board in 
  let board, p2 = repl_Piece p2 2 "first" board in
  let board, p1 = repl_piece p1 1 "second" board in 
  let board, p2 = repl_Piece p2 2 "second" board in
  match Game.Board.SmallBoard.build_settlement initial_board position, Game.Player.Player.build_settlment player_one position with
  | Some a, (Some b, _) -> repl_1 position ({player1 = b; player2 = player_two; board = a})
  | _ -> print_endline "SOMETHING VERY WRONG; ABORT; RETRY; DESTROY"
  ;











