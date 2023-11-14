open Game

let rec lst2str (lst : int list) : string =
  match lst with
  | h :: [] -> string_of_int h
  | h :: t -> string_of_int h ^ ", " ^ lst2str t
  | [] -> failwith "shouldn't happen"

(* read-eval-print loop for a piece placement
   if valid input: place piece, exit loop and print setup output
   if invalid input: rerun loop*)
let rec repl_piece (player : Game.Player.Player.t) (playernum : int)
    (piece : string) (board : Game.Board.SmallBoard.t) :
    Game.Board.SmallBoard.t * Game.Player.Player.t =
  let s =
    "Player " ^ string_of_int playernum ^ "'s" ^ piece ^ "Settlement > "
  in
  print_endline s;
  let input = int_of_string (read_line ()) in
  if input < 1 then repl_piece player playernum piece board
  else if input > 24 then repl_piece player playernum piece board
  else
    match
      ( Game.Board.SmallBoard.build_settlement board input,
        Game.Player.Player.build_settlment player input )
    with
    | Some a, (Some b, _) -> (a, b)
    | None, _ ->
        print_endline "There is already a piece at this settlment location";
        repl_piece player playernum piece board
    | _ ->
        failwith
          "UNEXPECTED BEHAVIOR TURN BACK NOW YOUR LIFE IS IN GREAT DANGER"

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
  print_endline
    "Possible locations are labeled 1-24 starting top left and increase right \
     then down\n";
  let p1 = Game.Player.Player.empty in
  let p2 = Game.Player.Player.empty in
  let board = Game.Board.SmallBoard.initial_board in
  let board, p1 = repl_piece p1 1 "first" board in
  let board, p2 = repl_piece p2 2 "first" board in
  let board, p2 = repl_piece p2 2 "second" board in
  let board, p1 = repl_piece p1 1 "second" board in
  print_endline "Board set up!";
  print_string "Player one has settlements at: ";
  print_string (lst2str (Game.Player.Player.get_settlement_locations p1));
  print_endline
    "Player one has the following in hand: 3 settlements; 15 roads; 3 cities; \
     0 resources";
  print_string "Player two has settlements at: ";
  print_string (lst2str (Game.Player.Player.get_settlement_locations p2));
  print_endline
    "Player one has the following in hand: 3 settlements; 15 roads; 3 cities; \
     0 resources"
