open Game
open Random

let rec lst2str (lst : int list) : string =
  match lst with
  | h :: [] -> string_of_int (h + 1)
  | h :: t -> string_of_int (h + 1) ^ ", " ^ lst2str t
  | [] -> failwith "shouldn't happen"

let rec turn (count: int) (players: Game.Player.Player.t * Game.Player.Player.t): unit = 
  let dice_roll = Random.int 6 + Random.int 6 in 
  print_string "Player "; print_int ((count mod 2) + 1); 
  print_string " has rolled a "; 
  print_int dice_roll; 
  print_endline "";
  if count mod 2 = 0 then (
  print_string "Player one has settlements at: ";
  print_endline (lst2str (Game.Player.Player.get_settlement_locations (fst players)));
  (* print_string "Player one has roads at: ";
  print_string (lst2str (Game.Player.Player.get_road_locations (fst players)));
  print_string "Player one has cities at: ";
  print_string (lst2str (Game.Player.Player.get_city_locations (fst players))); *)
  print_endline "Player one has in their hand: ";
  print_string "Settlements: ";
  print_endline (string_of_int (Game.Player.Player.get_settlement_count (fst players)));
  print_string "Roads: ";
  print_endline (string_of_int (Game.Player.Player.get_road_count (fst players)));
  print_string "Cities: ";
  print_endline (string_of_int (Game.Player.Player.get_city_count (fst players)));
  print_string "Clay: ";
  print_endline (string_of_int (Game.Player.Player.get_clay (fst players)));
  print_string "Wood: ";
  print_endline (string_of_int (Game.Player.Player.get_wood (fst players)));
  print_string "Wheat: ";
  print_endline (string_of_int (Game.Player.Player.get_wheat (fst players)));
  print_string "Sheep: ";
  print_endline (string_of_int (Game.Player.Player.get_sheep (fst players)));
  print_string "Ore: ";
  print_endline (string_of_int (Game.Player.Player.get_ore (fst players)));
  print_string "Dev Cards: ";
  print_endline (string_of_int (Game.Player.Player.get_dev_card_count (fst players)));
  print_string "Army Size: ";
  print_endline (string_of_int (Game.Player.Player.get_army_count (fst players)));
  print_string "Longest Road: ";
  print_endline (string_of_int (Game.Player.Player.get_long_road (fst players)));
  print_string "Victory Points: ";
  print_endline (string_of_int (Game.Player.Player.get_victory_points (fst players)));
  print_endline "";)
  else (
  print_string "Player one has settlements at: ";
  print_endline (lst2str (Game.Player.Player.get_settlement_locations (snd players)));
  (* print_string "Player one has roads at: ";
  print_string (lst2str (Game.Player.Player.get_road_locations (snd players)));
  print_string "Player one has cities at: ";
  print_string (lst2str (Game.Player.Player.get_city_locations (snd players))); *)
  print_endline "Player one has in their hand: ";
  print_string "Settlements: ";
  print_endline (string_of_int (Game.Player.Player.get_settlement_count (snd players)));
  print_string "Roads: ";
  print_endline (string_of_int (Game.Player.Player.get_road_count (snd players)));
  print_string "Cities: ";
  print_endline (string_of_int (Game.Player.Player.get_city_count (snd players)));
  print_string "Clay: ";
  print_endline (string_of_int (Game.Player.Player.get_clay (snd players)));
  print_string "Wood: ";
  print_endline (string_of_int (Game.Player.Player.get_wood (snd players)));
  print_string "Wheat: ";
  print_endline (string_of_int (Game.Player.Player.get_wheat (snd players)));
  print_string "Sheep: ";
  print_endline (string_of_int (Game.Player.Player.get_sheep (snd players)));
  print_string "Ore: ";
  print_endline (string_of_int (Game.Player.Player.get_ore (snd players)));
  print_string "Dev Cards: ";
  print_endline (string_of_int (Game.Player.Player.get_dev_card_count (snd players)));
  print_string "Army Size: ";
  print_endline (string_of_int (Game.Player.Player.get_army_count (snd players)));
  print_string "Longest Road: ";
  print_endline (string_of_int (Game.Player.Player.get_long_road (snd players)));
  print_string "Victory Points: ";
  print_endline (string_of_int (Game.Player.Player.get_victory_points (snd players)));
  print_endline "";);
  print_endline "";
  print_endline "Enter Anything to Continue";
  let input = read_line () in print_endline ""

(* read-eval-print loop for a road placement
   if valid input: place road, exit loop and print setup output
   if invalid input: rerun loop*)
let rec repl_road (player : Game.Player.Player.t) (playernum : int)
    (just_placed : int) (board : Game.Board.SmallBoard.t) :
    Game.Board.SmallBoard.t * Game.Player.Player.t =
  let s =
    "Where would you like your road to be? \n\
    \ Possible locations are labeled 1-30 starting top left and increase right \
     then down  "
  in
  print_endline s;
  let input = int_of_string (read_line ()) in
  if input < 1 then repl_road player playernum just_placed board
  else if input > 30 then repl_road player playernum just_placed board
  else if
    (not
       ((List.nth (Game.Board.SmallBoard.get_edge_lst board) input).node1
       = List.nth (Game.Board.SmallBoard.get_node_lst board) (just_placed - 1)))
    && not
         ((List.nth (Game.Board.SmallBoard.get_edge_lst board) input).node2
         = List.nth (Game.Board.SmallBoard.get_node_lst board) (just_placed - 1)
         )
  then repl_road player playernum just_placed board
  else
    match
      ( Game.Board.SmallBoard.build_road board input,
        Game.Player.Player.build_road player input )
    with
    | Some a, (Some b, _) -> (a, b)
    | _ ->
        failwith
          "UNEXPECTED BEHAVIOR TURN BACK NOW YOUR LIFE IS IN GREAT DANGER"

let rec repl_piece (player : Game.Player.Player.t) (playernum : int)
    (piece : string) (board : Game.Board.SmallBoard.t) :
    Game.Board.SmallBoard.t * Game.Player.Player.t * int =
  let s =
    "Player " ^ string_of_int playernum ^ "'s " ^ piece ^ " Settlement: "
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
    | Some a, (Some b, _) -> (a, b, input)
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
  let board, p1, just_placed = repl_piece p1 1 "first" board in
  (* let board, p1 = repl_road p1 1 just_placed board in *)
  let board, p2, just_placed = repl_piece p2 2 "first" board in
  (* let board, p1 = repl_road p2 2 just_placed board in *)
  let board, p2, just_placed = repl_piece p2 2 "second" board in
  (* let board, p1 = repl_road p2 2 just_placed board in *)
  let board, p1, just_placed = repl_piece p1 1 "second" board in
  (* let board, p1 = repl_road p1 1 just_placed board in *)
  print_endline "Board set up!";
  print_string "Player one has settlements at: ";
  print_endline (lst2str (Game.Player.Player.get_settlement_locations p1));
  (* print_string "and roads at: ";
  print_string (lst2str (Game.Player.Player.get_road_locations p1));*)
  print_endline 
    "Player one has the following in hand: 3 settlements; 15 roads; 3 cities; \
     0 resources";
  print_string "Player two has settlements at: ";
  print_endline (lst2str (Game.Player.Player.get_settlement_locations p2));
  (* print_string "and roads at: ";
  print_string (lst2str (Game.Player.Player.get_road_locations p2)); *)
  print_endline
    "Player one has the following in hand: 3 settlements; 15 roads; 3 cities; \
     0 resources";
  
  print_endline "Enter Anything to Continue"; read_line ();

  let x = ref 0 in
  while !x<10 do 
    turn !x (p1, p2);
    x:=!x+1;
  done;

  
  
