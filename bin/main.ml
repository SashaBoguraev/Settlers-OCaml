open Game
open Random

let rec lst2str (lst : int list) : string =
  match lst with
  | h :: [] -> string_of_int (h + 1)
  | h :: t -> string_of_int (h + 1) ^ ", " ^ lst2str t
  | [] -> "None"

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
  let cities = Game.Player.Player.get_city_locations player in
  let entities = settlements @ cities @ cities in
  let player_rec_one =
    loop_settlements_resource_one node_list entities player roll
  in
  let player_rec_two =
    loop_settlements_resource_two node_list entities player_rec_one roll
  in
  let player_rec_three =
    loop_settlements_resource_three node_list entities player_rec_two roll
  in
  player_rec_three

(* read-eval-print loop for a road placement
   if valid input: place road, exit loop and print setup output
   if invalid input: rerun loop*)
let rec repl_road (player : Game.Player.Player.t)
    (board : Game.Board.SmallBoard.t) (setup:bool):
    Game.Board.SmallBoard.t * Game.Player.Player.t =
  let s =
    "Where would you like your road to be?"
  in
  print_endline s;
  if (not setup) then print_endline "Type -1 to cancel build";

  let input =
    try int_of_string (read_line ()) with
    | Failure _ -> 100
    | _ -> int_of_string (read_line ())
  in
  if input = -1 && not setup then (board, player)
else if input = 100 then (
    match input with
    | _ ->
        print_endline "INVALID INPUT";
        repl_road player board setup)
  else if input > 30 || input < 1 then (print_endline "Out of range try again"; repl_road player board setup)
  else
    match
      ( Game.Board.SmallBoard.build_road board input,
        Game.Player.Player.build_road player input )
    with
    | Some a, (Some b, _) -> (a, b)
    | _, (None, _) ->
        print_endline "Not a valid road location";
        repl_road player board setup
    | None, _ ->
        print_endline "There is already a road here";
        repl_road player board setup

let rec repl_piece (player : Game.Player.Player.t) (playernum : int)
    (piece : string) (board : Game.Board.SmallBoard.t) (setup :bool):
    Game.Board.SmallBoard.t * Game.Player.Player.t * int =
  print_string "Player ";
  print_int playernum;
  print_string ": ";
  let s =
    "Where would you like your settlement to be?  "
  in
  print_endline s;
  if (setup = false) then print_endline "Type -1 to cancel build.";
  let input =
    try int_of_string (read_line ()) with
    | Failure _ -> 100
    | _ -> int_of_string (read_line ())
  in
  if input = -1 && (not setup )then (board, player, playernum)
else if input = 100 then (
    match input with
    | _ ->
        print_endline "INVALID INPUT";
        repl_piece player playernum piece board setup)
  else if input > 24 || input < 1 then (print_endline "Out of range try again";repl_piece player playernum piece board setup)
  else
    match
      ( Game.Board.SmallBoard.build_settlement board input,
        Game.Player.Player.build_settlment player input )
    with
    | Some a, (Some b, _) -> (a, Game.Player.Player.add_point b, input)
    | None, _ ->
        print_endline "There is already a piece at this settlement location";
        repl_piece player playernum piece board setup
    | _, (None, i) -> match i with | 3 -> (print_endline "not a possible location"; repl_piece player playernum piece board setup)
    | _ ->
        failwith
          "UNEXPECTED BEHAVIOR TURN BACK NOW YOUR LIFE IS IN GREAT DANGER"

let rec repl_city (player : Game.Player.Player.t)
    (board : Game.Board.SmallBoard.t) :
    Game.Board.SmallBoard.t * Game.Player.Player.t =
  let s = "Where would you like your city to be? \nType -1 to cancel. Possible locations are  " in
  print_endline s;
  print_endline (lst2str (Game.Player.Player.get_settlement_locations player));
  let input =
    try int_of_string (read_line ()) with
    | Failure _ -> 100
    | _ -> int_of_string (read_line ())
  in
  if input = -1 then (board, player)
else if input = 100 then (
    (match input with
    | _ ->
        print_endline "INVALID INPUT";
        repl_city player board))
  else if
    not
      (List.mem (input - 1)
         (Game.Player.Player.get_settlement_locations player))
  then (
    match input with
    | _ ->
        print_endline "No settlement there";
        repl_city player board)
  else
    match
      ( Game.Board.SmallBoard.build_city board input,
        Game.Player.Player.build_city player input )
    with
    | Some a, (Some b, _) -> (a, Game.Player.Player.add_point b)
    | _, (None, _) ->
        print_endline "Not a valid city location";
        repl_city player board
    | None, _ ->
        print_endline "There is already a city here";
        repl_city player board


let rec repl_trade (player : Game.Player.Player.t) : Game.Player.Player.t = 
  let s = "What resourse would you like to trade in? Type 'cancel' to cancel " in
  print_endline s; let input = read_line () in
  match input with
  | s -> if String.lowercase_ascii s = "cancel" then player
  else if String.lowercase_ascii s = "wheat" then (if (Game.Player.Player.get_wheat player > 2) then (
                              print_endline "What resource would you like: ";
                              let input1 = read_line () in match input1 with | s1 -> let new_player = Game.Player.Player.trade_wheat player in if String.lowercase_ascii s1 = "wood" then Game.Player.Player.add_wood new_player
                                                                                    else if String.lowercase_ascii s1 = "ore" then Game.Player.Player.add_ore new_player
                                                                                    else if String.lowercase_ascii s1 = "sheep" then Game.Player.Player.add_sheep new_player
                                                                                    else if String.lowercase_ascii s1 = "clay" then Game.Player.Player.add_clay new_player
                                                                                    else if String.lowercase_ascii s1 = "wheat" then Game.Player.Player.add_wheat new_player
                                                                                    else (print_endline "Invalid Input"; repl_trade player))
                                                                                  else (print_endline "Don't have enough of that resourse"; repl_trade player))
        else if String.lowercase_ascii s = "ore" then (if (Game.Player.Player.get_ore player > 2) then (
          print_endline "What resource would you like: ";
          let input1 = read_line () in match input1 with | s1 -> let new_player = Game.Player.Player.trade_ore player in if String.lowercase_ascii s1 = "wood" then Game.Player.Player.add_wood new_player
                                                                else if String.lowercase_ascii s1 = "ore" then Game.Player.Player.add_ore new_player
                                                                else if String.lowercase_ascii s1 = "sheep" then Game.Player.Player.add_sheep new_player
                                                                else if String.lowercase_ascii s1 = "clay" then Game.Player.Player.add_clay new_player
                                                                else if String.lowercase_ascii s1 = "wheat" then Game.Player.Player.add_wheat new_player
                                                                else (print_endline "Invalid Input"; repl_trade player))
                                                              else (print_endline "Don't have enough of that resourse"; repl_trade player))
        else if String.lowercase_ascii s = "sheep" then (if (Game.Player.Player.get_sheep player > 2) then (
          print_endline "What resource would you like: ";
          let input1 = read_line () in match input1 with | s1 -> let new_player = Game.Player.Player.trade_sheep player in if String.lowercase_ascii s1 = "wood" then Game.Player.Player.add_wood new_player
                                                                else if String.lowercase_ascii s1 = "ore" then Game.Player.Player.add_ore new_player
                                                                else if String.lowercase_ascii s1 = "sheep" then Game.Player.Player.add_sheep new_player
                                                                else if String.lowercase_ascii s1 = "clay" then Game.Player.Player.add_clay new_player
                                                                else if String.lowercase_ascii s1 = "wheat" then Game.Player.Player.add_wheat new_player
                                                                else (print_endline "Invalid Input"; repl_trade player))
                                                              else (print_endline "Don't have enough of that resourse"; repl_trade player))
        else if String.lowercase_ascii s = "clay" then (if (Game.Player.Player.get_clay player > 2) then (
          print_endline "What resource would you like: ";
          let input1 = read_line () in match input1 with | s1 -> let new_player = Game.Player.Player.trade_clay player in if String.lowercase_ascii s1 = "wood" then Game.Player.Player.add_wood new_player
                                                                else if String.lowercase_ascii s1 = "ore" then Game.Player.Player.add_ore new_player
                                                                else if String.lowercase_ascii s1 = "sheep" then Game.Player.Player.add_sheep new_player
                                                                else if String.lowercase_ascii s1 = "clay" then Game.Player.Player.add_clay new_player
                                                                else if String.lowercase_ascii s1 = "wheat" then Game.Player.Player.add_wheat new_player
                                                                else (print_endline "Invalid Input"; repl_trade player))
                                                              else (print_endline "Don't have enough of that resourse"; repl_trade player))
        else if String.lowercase_ascii s = "wood" then (if (Game.Player.Player.get_wood player > 2) then (
          print_endline "What resource would you like: ";
          let input1 = read_line () in match input1 with | s1 -> let new_player = Game.Player.Player.trade_wood player in if String.lowercase_ascii s1 = "wood" then Game.Player.Player.add_wood new_player
                                                                else if String.lowercase_ascii s1 = "ore" then Game.Player.Player.add_ore new_player
                                                                else if String.lowercase_ascii s1 = "sheep" then Game.Player.Player.add_sheep new_player
                                                                else if String.lowercase_ascii s1 = "clay" then Game.Player.Player.add_clay new_player
                                                                else if String.lowercase_ascii s1 = "wheat" then Game.Player.Player.add_wheat new_player
                                                                else (print_endline "Invalid Input"; repl_trade player))
                                                              else (print_endline "Don't have enough of that resourse"; repl_trade player))
        else (print_endline "Invalid Input"; repl_trade player)
        

let rec build (player : Game.Player.Player.t)(board : Game.Board.SmallBoard.t)
    (player_num : int) : Game.Player.Player.t * Game.Board.SmallBoard.t=
  print_endline
    "Enter 1 to build a road, 2 to build a settlement, 3 to build a city, 4 to trade, or 5 \
     to end turn: ";
  let input =
    try int_of_string (read_line ()) with
    | Failure _ -> 0
    | _ -> int_of_string (read_line ())
  in
  if input = 0 then (
    match input with
    | _ ->
        print_endline "INVALID INPUT";
        build player board player_num)
  else if input < 1 || input > 5 then build player board player_num
  else if input = 1 then if ((Game.Player.Player.get_road_count player > 0) && (Game.Player.Player.get_clay player > 0) && (Game.Player.Player.get_wood player > 0)) then (
    let road_build = repl_road player board false in
    match road_build with
    | _ ->
        print_endline "Success!";
        build (snd road_build) (fst road_build) player_num) else (print_endline "Not enough resources to do this"; build player board player_num)
  else if input = 2 then if ((Game.Player.Player.get_settlement_count player > 0) && (Game.Player.Player.get_clay player > 0) && (Game.Player.Player.get_wood player > 0)&& (Game.Player.Player.get_wheat player > 0)&& (Game.Player.Player.get_sheep player > 0)) then(
    let settlement_build = repl_piece player player_num "next" board false in
    match settlement_build with
    | b, p, i ->
        print_endline "Success!";
        build p b player_num) else (print_endline "Not enough resources to do this"; build player board player_num)
  else if input = 3 then if ((Game.Player.Player.get_city_count player > 0) && (Game.Player.Player.get_wheat player > 1) && (Game.Player.Player.get_ore player > 2)) then(
    let city_build = repl_city player board in
    match city_build with
    | _ ->
        print_endline "Success!";
        build (snd city_build) (fst city_build) player_num) else (print_endline "Not enough resources to do this"; build player board player_num)
    else if input = 4 then if ((Game.Player.Player.get_clay player > 2) || (Game.Player.Player.get_wood player > 2) || (Game.Player.Player.get_wheat player > 2)|| (Game.Player.Player.get_sheep player > 2)|| (Game.Player.Player.get_ore player > 2)) then let trade_player =  (repl_trade player) in match trade_player with | _ -> build trade_player board player_num 
                          else (print_endline "Not enough resources to do this"; build player board player_num)
  else
    match input with
    | _ ->
        print_endline "Turn ended";
        (player, board)
  
let check_input input = match input with
| "QUIT" -> None
| _ -> Some input

let print_rules (u : unit) : unit = (
  print_endline "====SETUP====\n";
  print_endline "Player 1 will place a settlement down first at a location of their choosing\n";
  print_endline "Player 1 will then place a road off their first settlement\n";
  print_endline "Player 2 will then place a settlement down at a location of their choosing\n";
  print_endline "Player 2 will then place a road off their first settlement\n";
  print_endline "Player 2 will then place a settlement down at a location of their choosing\n";
  print_endline "Player 2 will then place a road off their second settlement\n";
  print_endline "Player 1 will then place a settlement down at a location of their choosing\n";
  print_endline "Player 1 will then place a road off their second settlement\n";
  print_endline "\n====TURNS====\n";
  print_endline "After setup, turn play will start with player 1, alternating turns after that \n";
  print_endline "On each turn, a dice will be rolled and update player resources based on the number of settlements a player has on a tile \n";
  print_endline "Each settlement on the tile that is rolled returns 1 of that resource to a player, each city returns 2 \n";
  print_endline "The player who's turn it is then has the option to build, trade, or end turn \n";
  print_endline "They can build either a settlement, road, or city if they have the allocated resources, or trade 3 of the same resource for 1 of another \n";
  print_endline "A road costs: 1 clay, 1 wood \n";
  print_endline "A settlement costs: 1 clay, 1 wood, 1 wheat, 1 sheep \n";
  print_endline "A city costs: 2 wheat, 3 ore \n";
  print_endline "\n====RULES====\n";
  print_endline "1. No two settlements can be placed at the same location \n";
  print_endline "2. No two roads can be placed at the same location \n";
  print_endline "3. No two cities can be placed at the same location \n";
  print_endline "4. Roads can only be built off a players settlement or other road \n";
  print_endline "5. Settlements must be built off a players road \n";
  print_endline "6. Cities replace settlements, they do not build off roads \n";
  print_endline "\n Winning the game \n";
  print_endline "A player has won the game when they reach 5 Victory Points \n";
  print_endline "A settlement is worth 1 Victory Point and a city is worth 2 Victory Points \n";
  print_endline "The setup settlements each count towards VP total \n";
)

let print_board (u : unit) : unit = (
print_endline "            1        2";
print_endline "           / \\     / \\";
print_endline "        1 /  2\\  3/  4\\";
print_endline "         /     \\ /     \\";
print_endline "        3       4       5 ";
print_endline "       5| Sheep | Wood  |7 ";
print_endline "        |   6   |   4   | ";
print_endline "        6       7       8 ";
print_endline "       / \\     / \\     / \\";
print_endline "    8 /  9\\ 10/ 11\\ 12/ 13\\";
print_endline "     /     \\ /     \\ /     \\";
print_endline "    9       10      11      12";
print_endline "    | Clay  | Desert|  Wood |";
print_endline "    |   3   |       |   2   |";
print_endline "  14|     15|     16|     17|";
print_endline "   13       14      15      16";
print_endline "     \\     / \\     / \\     /";
print_endline "    18\\ 19/ 20\\ 21/ 22\\ 23/";
print_endline "       \\ /     \\ /     \\ /";
print_endline "        17      18      19 ";
print_endline "        | Wheat |  Ore  | ";
print_endline "      24|   5   |   1   |26 ";
print_endline "        20      21      22 ";
print_endline "         \\     / \\     / ";
print_endline "        27\\ 28/ 29\\ 30/ ";
print_endline "           \\ /     \\ / ";
print_endline "           23       24 \n";)


let rec turn (count : int)
    (players : Game.Player.Player.t * Game.Player.Player.t)
    (board : Game.Board.SmallBoard.t) :
    Game.Player.Player.t * Game.Player.Player.t * Game.Board.SmallBoard.t =
  let dice_roll = Random.int 6 + 1 in
  print_endline "";
  print_endline "";
  print_string "Player ";
  print_int ((count mod 2) + 1);
  print_string " has rolled a ";
  print_int dice_roll;
  print_endline "";
  print_endline "\n ====BOARD==== \n";
  print_board ();
  let node_list = Game.Board.SmallBoard.get_node_lst board in
  let new_player_one = assign_resources node_list (fst players) dice_roll in
  print_string "Player one has settlements at: ";
  print_endline
    (lst2str (Game.Player.Player.get_settlement_locations new_player_one));
  print_string "Player one has roads at: ";
  print_endline
    (lst2str (Game.Player.Player.get_road_locations new_player_one));
  print_string "Player one has cities at: ";
  print_endline
    (lst2str (Game.Player.Player.get_city_locations new_player_one));
  print_endline "Player one has in their hand: ";
  print_string "Settlements: ";
  print_endline
    (string_of_int (Game.Player.Player.get_settlement_count new_player_one));
  print_string "Roads: ";
  print_endline (string_of_int (Game.Player.Player.get_road_count new_player_one));
  print_string "Cities: ";
  print_endline (string_of_int (Game.Player.Player.get_city_count new_player_one));
  print_string "Clay: ";
  print_endline (string_of_int (Game.Player.Player.get_clay new_player_one));
  print_string "Wood: ";
  print_endline (string_of_int (Game.Player.Player.get_wood new_player_one));
  print_string "Wheat: ";
  print_endline (string_of_int (Game.Player.Player.get_wheat new_player_one));
  print_string "Sheep: ";
  print_endline (string_of_int (Game.Player.Player.get_sheep new_player_one));
  print_string "Ore: ";
  print_endline (string_of_int (Game.Player.Player.get_ore new_player_one));
  print_string "Victory Points: ";
  print_endline
    (string_of_int (Game.Player.Player.get_victory_points new_player_one));

  print_endline "";
  print_endline "";
  let new_player_two = assign_resources node_list (snd players) dice_roll in
    print_string "Player two has settlements at: ";
    print_endline
      (lst2str (Game.Player.Player.get_settlement_locations new_player_two));
    print_string "Player two has roads at: ";
    print_endline
      (lst2str (Game.Player.Player.get_road_locations new_player_two));
    print_string "Player two has cities at: ";
    print_endline
      (lst2str (Game.Player.Player.get_city_locations new_player_two));
    print_endline "Player two has in their hand: ";
    print_string "Settlements: ";
    print_endline
      (string_of_int (Game.Player.Player.get_settlement_count new_player_two));
    print_string "Roads: ";
    print_endline (string_of_int (Game.Player.Player.get_road_count new_player_two));
    print_string "Cities: ";
    print_endline (string_of_int (Game.Player.Player.get_city_count new_player_two));
    print_string "Clay: ";
    print_endline (string_of_int (Game.Player.Player.get_clay new_player_two));
    print_string "Wood: ";
    print_endline (string_of_int (Game.Player.Player.get_wood new_player_two));
    print_string "Wheat: ";
    print_endline (string_of_int (Game.Player.Player.get_wheat new_player_two));
    print_string "Sheep: ";
    print_endline (string_of_int (Game.Player.Player.get_sheep new_player_two));
    print_string "Ore: ";
    print_endline (string_of_int (Game.Player.Player.get_ore new_player_two));
    print_string "Victory Points: ";
    print_endline
      (string_of_int (Game.Player.Player.get_victory_points new_player_two));

    print_endline "";
    print_endline "";
  if count mod 2 = 0 then (
    let p, b = build new_player_one board 1 in
    (p, new_player_two, b))
  else
    let p, b = build new_player_two board 2 in
    (new_player_one, p, b)

let rec repl_turn turn_number player1 player2 board : string * Game.Player.Player.t =
  if (Game.Player.Player.get_victory_points player1) >= 5 && (Game.Player.Player.get_victory_points player2) >= 5 then "There is a draw!", player2
  else if (Game.Player.Player.get_victory_points player1) >= 5 then "The winner is Player 1", player1
  else if (Game.Player.Player.get_victory_points player2) >= 5 then "The winner is Player 2", player2
  else
    let p1, p2, board_new = 
    turn turn_number (player1, player2) board in
    repl_turn (turn_number + 1) p1 p2 board_new

let () =
  print_endline "Welcome to Catan!\n";
  print_rules ();
  print_board ();

  print_endline
    "Possible settlement locations are labeled 1-24 starting top left and increase right \
     then down.\nPossible road locations are labeled 1-30 starting top left and increase right then down.";
  let p1 = Game.Player.Player.empty in
  let p2 = Game.Player.Player.empty in
  let board = Game.Board.SmallBoard.initial_board in
  let board, p1, just_placed = repl_piece p1 1 "first" board true in
  let board, p1 = repl_road p1 board true in
  let board, p2, just_placed = repl_piece p2 2 "first" board true in
  let board, p2 = repl_road p2 board true in
  let board, p2, just_placed = repl_piece p2 2 "second" board true in
  let board, p2 = repl_road p2 board true in
  let board, p1, just_placed = repl_piece p1 1 "second" board true in
  let board, p1 = repl_road p1 board true in
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

  let x = 0 in
  let message, winner = repl_turn x p1 p2 board in
  print_endline message