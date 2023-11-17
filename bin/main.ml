open Game
open Random

let rec lst2str (lst : int list) : string =
  match lst with
  | h :: [] -> string_of_int (h + 1)
  | h :: t -> string_of_int (h + 1) ^ ", " ^ lst2str t
  | [] -> failwith "shouldn't happen"

let update_resource player (resource : Game.Board.resource option) :
    Game.Player.Player.t =
  match resource with
  | Some Wheat -> Game.Player.Player.add_wheat player
  | Some Wood -> Game.Player.Player.add_wood player
  | Some Ore -> Game.Player.Player.add_ore player
  | Some Sheep -> Game.Player.Player.add_sheep player
  | Some Clay -> Game.Player.Player.add_clay player
  | None -> player

(* Loops through the node list and returns a new player with all the correct additional resources from the first component of all their nodes *)
let rec loop_settlements_resource_one node_list settlements player roll =
  match settlements with
  | [] -> player
  | h :: t ->
      let loc = List.nth node_list h in
      if Game.Board.SmallBoard.get_node_border_one_number loc = Some roll then
        let resource_one =
          Game.Board.SmallBoard.get_node_border_one_resource loc
        in
        let new_player = update_resource player resource_one in
        loop_settlements_resource_one node_list t new_player roll
      else loop_settlements_resource_one node_list t player roll

(* Loops through the node list and returns a new player with all the correct additional resources from the second component of all their nodes *)
let rec loop_settlements_resource_two node_list settlements player roll =
  match settlements with
  | [] -> player
  | h :: t ->
      let loc = List.nth node_list h in
      if Game.Board.SmallBoard.get_node_border_two_number loc = Some roll then
        let resource_two =
          Game.Board.SmallBoard.get_node_border_two_resource loc
        in
        let new_player = update_resource player resource_two in
        loop_settlements_resource_two node_list t new_player roll
      else loop_settlements_resource_two node_list t player roll

(* Loops through the node list and returns a new player with all the correct additional resources from the third component of all their nodes *)
let rec loop_settlements_resource_three node_list settlements player roll =
  match settlements with
  | [] -> player
  | h :: t ->
      let loc = List.nth node_list h in
      if Game.Board.SmallBoard.get_node_border_three_number loc = Some roll then
        let resource_three =
          Game.Board.SmallBoard.get_node_border_three_resource loc
        in
        let new_player = update_resource player resource_three in
        loop_settlements_resource_three node_list t new_player roll
      else loop_settlements_resource_three node_list t player roll

(* Uses above helper functions to get correct resources for players *)
let assign_resources node_list player roll : Game.Player.Player.t =
  let settlements = Game.Player.Player.get_settlement_locations player in
  let player_rec_one =
    loop_settlements_resource_one node_list settlements player roll
  in
  let player_rec_two =
    loop_settlements_resource_two node_list settlements player_rec_one roll
  in
  let player_rec_three =
    loop_settlements_resource_three node_list settlements player_rec_two roll
  in
  player_rec_three

let rec turn (count : int)
    (players : Game.Player.Player.t * Game.Player.Player.t)
    (board : Game.Board.SmallBoard.t) :
    Game.Player.Player.t * Game.Player.Player.t * Game.Board.SmallBoard.t =
  let dice_roll = Random.int 6 + Random.int 6 in
  print_string "Player ";
  print_int ((count mod 2) + 1);
  print_string " has rolled a ";
  print_int dice_roll;
  print_endline "";
  let node_list = Game.Board.SmallBoard.get_node_lst board in
  if count mod 2 = 0 then (
    let new_player = assign_resources node_list (fst players) dice_roll in
    print_string "Player one has settlements at: ";
    print_endline
      (lst2str (Game.Player.Player.get_settlement_locations new_player));
    (* print_string "Player one has roads at: ";
       print_string (lst2str (Game.Player.Player.get_road_locations new_player));
       print_string "Player one has cities at: ";
       print_string (lst2str (Game.Player.Player.get_city_locations new_player)); *)
    print_endline "Player one has in their hand: ";
    print_string "Settlements: ";
    print_endline
      (string_of_int (Game.Player.Player.get_settlement_count new_player));
    print_string "Roads: ";
    print_endline (string_of_int (Game.Player.Player.get_road_count new_player));
    print_string "Cities: ";
    print_endline (string_of_int (Game.Player.Player.get_city_count new_player));
    print_string "Clay: ";
    print_endline (string_of_int (Game.Player.Player.get_clay new_player));
    print_string "Wood: ";
    print_endline (string_of_int (Game.Player.Player.get_wood new_player));
    print_string "Wheat: ";
    print_endline (string_of_int (Game.Player.Player.get_wheat new_player));
    print_string "Sheep: ";
    print_endline (string_of_int (Game.Player.Player.get_sheep new_player));
    print_string "Ore: ";
    print_endline (string_of_int (Game.Player.Player.get_ore new_player));
    print_string "Dev Cards: ";
    print_endline
      (string_of_int (Game.Player.Player.get_dev_card_count new_player));
    print_string "Army Size: ";
    print_endline (string_of_int (Game.Player.Player.get_army_count new_player));
    print_string "Longest Road: ";
    print_endline (string_of_int (Game.Player.Player.get_long_road new_player));
    print_string "Victory Points: ";
    print_endline
      (string_of_int (Game.Player.Player.get_victory_points new_player));
    print_endline "";
    print_endline "";
    print_endline "Enter Anything to Continue";
    let input = read_line () in
    (new_player, snd players, board))
  else
    let new_player = assign_resources node_list (snd players) dice_roll in
    print_string "Player two has settlements at: ";
    print_endline
      (lst2str (Game.Player.Player.get_settlement_locations new_player));
    (* print_string "Player one has roads at: ";
       print_string (lst2str (Game.Player.Player.get_road_locations new_player));
       print_string "Player one has cities at: ";
       print_string (lst2str (Game.Player.Player.get_city_locations new_player)); *)
    print_endline "Player two has in their hand: ";
    print_string "Settlements: ";
    print_endline
      (string_of_int (Game.Player.Player.get_settlement_count new_player));
    print_string "Roads: ";
    print_endline (string_of_int (Game.Player.Player.get_road_count new_player));
    print_string "Cities: ";
    print_endline (string_of_int (Game.Player.Player.get_city_count new_player));
    print_string "Clay: ";
    print_endline (string_of_int (Game.Player.Player.get_clay new_player));
    print_string "Wood: ";
    print_endline (string_of_int (Game.Player.Player.get_wood new_player));
    print_string "Wheat: ";
    print_endline (string_of_int (Game.Player.Player.get_wheat new_player));
    print_string "Sheep: ";
    print_endline (string_of_int (Game.Player.Player.get_sheep new_player));
    print_string "Ore: ";
    print_endline (string_of_int (Game.Player.Player.get_ore new_player));
    print_string "Dev Cards: ";
    print_endline
      (string_of_int (Game.Player.Player.get_dev_card_count new_player));
    print_string "Army Size: ";
    print_endline (string_of_int (Game.Player.Player.get_army_count new_player));
    print_string "Longest Road: ";
    print_endline (string_of_int (Game.Player.Player.get_long_road new_player));
    print_string "Victory Points: ";
    print_endline
      (string_of_int (Game.Player.Player.get_victory_points new_player));
    print_endline "";
    print_endline "";
    print_endline "Enter Anything to Continue";
    let input = read_line () in
    (fst players, new_player, board)

(* read-eval-print loop for a road placement
   if valid input: place road, exit loop and print setup output
   if invalid input: rerun loop*)
let rec repl_road (player : Game.Player.Player.t)
    (board : Game.Board.SmallBoard.t) :
    Game.Board.SmallBoard.t * Game.Player.Player.t =
  let s =
    "Where would you like your road to be? \n\
    \ Possible locations are labeled 1-30 starting top left and increase right \
     then down  "
  in
  print_endline s;
  let input = try int_of_string (read_line ()) with | Failure("int_of_string") -> 0
                                                    | _ -> int_of_string (read_line ()) in
  if input = 0 then match input with | _ -> print_endline "INVALID INPUT";
  repl_road player board
  else if input > 30 || input < 1 then repl_road player board
  else
    match
      ( Game.Board.SmallBoard.build_road board input,
        Game.Player.Player.build_road player input )
    with
    | Some a, (Some b, _) -> (a, b)
    | _, (None, _) ->
        print_endline "Not a valid road location";
        repl_road player board
    | None, _ ->
        print_endline "There is already a road here";
        repl_road player board

let rec repl_turn turn_number player1 player2 board : Game.Player.Player.t =
  if turn_number > 10 then player1
  else
    let p1, p2, board_new = turn turn_number (player1, player2) board in
    repl_turn (turn_number + 1) p1 p2 board_new

let rec repl_piece (player : Game.Player.Player.t) (playernum : int)
    (piece : string) (board : Game.Board.SmallBoard.t) :
    Game.Board.SmallBoard.t * Game.Player.Player.t * int =
  let s =
    "Player " ^ string_of_int playernum ^ "'s " ^ piece ^ " Settlement: "
  in
  print_endline s;
  let input = try int_of_string (read_line ()) with | Failure("int_of_string") -> 0
                                                    | _ -> int_of_string (read_line ())
in
  if input = 0 then match input with | _ -> print_endline "INVALID INPUT";
  repl_piece player playernum piece board
  else if input > 24 || input < 1 then repl_piece player playernum piece board
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
  let board, p1 = repl_road p1 board in

  let board, p2, just_placed = repl_piece p2 2 "first" board in
  let board, p2 = repl_road p2 board in
  let board, p2, just_placed = repl_piece p2 2 "second" board in
  let board, p2 = repl_road p2 board in
  let board, p1, just_placed = repl_piece p1 1 "second" board in
  let board, p1 = repl_road p1 board in
  print_endline "Board set up!";
  print_string "Player one has settlements at: ";
  print_string (lst2str (Game.Player.Player.get_settlement_locations p1));
  print_string " and roads at: ";
  print_endline (lst2str (Game.Player.Player.get_road_locations p1));
  print_endline
    "Player one has the following in hand: 3 settlements; 15 roads; 3 cities; \
     0 resources";
  print_string "Player two has settlements at: ";
  print_string (lst2str (Game.Player.Player.get_settlement_locations p2));
  print_string " and roads at: ";
  print_endline (lst2str (Game.Player.Player.get_road_locations p2));
  print_endline
    "Player one has the following in hand: 3 settlements; 15 roads; 3 cities; \
     0 resources";

  print_endline "Enter Anything to Continue";
  read_line ();

  let x = 0 in
  let winner = repl_turn x p1 p2 board in
  print_endline "The winner is player ";
  print_int 1
