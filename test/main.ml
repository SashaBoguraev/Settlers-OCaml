(* Test plan:

OUnit tests both of the files in lib, lib/board.ml and lib/player.ml.
The file in bin, bin/main.exe, is tested manually. 

lib/board.ml has two main purposes: creating the initial board and 
building on an existing board, if and only if it’s legal. We did not
test the creation of every single edge and node in the initial 
board to avoid redundant testing. We use mostly glass box testing to
test all of our building functions in lib/board.ml so that we make
sure to cover every reason that a build would or would not be legal
and verify the expected output. We use some black box testing to
test elementary build actions that we know are legal and some
initial board attributes. We chose not to do any randomized testing 
for lib/board.ml because of the nature of the game, where there are 
only a few valid building locations at a given time (if any), and so 
we were able to test all possible actions ourselves.

In lib/player.ml, we maintain all attributes of an active player and 
update and get these. It also maintains a list of legal moves for a 
player. We use glass box unit testing to attempt to break each of 
these rules and verify that lib/player.ml responds accordingly, as 
well as making all legal moves. Additionally, we test black box general 
cases that each player should have to complete during setup. Again, we 
did not write any randomized tests because of the nature of the game 
and what is allowed.

bin/main.exe was tested manually rather than using OUnit. Mostly, 
we tested this file using blackbox tests while playing the game normally, 
but we also tried to enter extraneous input (e.g. strings when integers 
were expected), or break the rules by making illegal moves to verify 
that our program would respond accordingly.

We believe that this approach demonstrates functionality because almost 
every function used while the game is being executed (the functions in 
bin/main.exe) is covered by glass box OUnit tests, and then again 
indirectly by manual tests when they are called in bin/main.exe. 
The remaining functions are the ones that are defined only in 
bin/main.exe. Although they have no unit tests, because they are made 
up combinations of calls functions from the Player and Board Modules 
(which have OUnit coverage), we are able to confirm that they work 
by running bin/main.exe and testing them manually. This in turn 
provides improved testing of the functions in lib/board.ml and 
lib/player.ml. By combining an array of testing methods, we are 
able to demonstrate that our functions both fulfill specifications 
and handle extraneous input well. *)

open OUnit2
open Game

(********************************************************************
   Here are some helper functions for testing players and boards
 ********************************************************************)
module Test_Player = Game.Player

let get_player_from_option (p : 'a option) : 'a =
  match p with Some x -> x | None -> failwith "this option had no player"

let rec add_n_resources (f : Player.Player.t -> Player.Player.t)
    (player : Player.Player.t) (n : int) : Player.Player.t =
  match n with 1 -> f player | n -> add_n_resources f (f player) (n - 1)

let rec build_n (f : Player.Player.t -> int -> Player.Player.t option * int)
    (player : Player.Player.t) (n : int) : Player.Player.t =
  match n with
  | 1 -> get_player_from_option (fst (f player n))
  | n -> build_n f (get_player_from_option (fst (f player n))) (n - 1)

let empty_player = Test_Player.Player.empty

let player_three_all = 
  let with_wood = add_n_resources Player.Player.add_wood empty_player 3 in
  let with_clay = add_n_resources Player.Player.add_clay with_wood 3 in
  let with_wheat = add_n_resources Player.Player.add_wheat with_clay 3 in
  let with_ore = add_n_resources Player.Player.add_ore with_wheat 3 in
  let with_sheep = add_n_resources Player.Player.add_sheep with_ore 3 in
  with_sheep


let player_that_can_build_16_roads : Player.Player.t =
  let with_wood = add_n_resources Player.Player.add_wood empty_player 12 in
  let with_both = add_n_resources Player.Player.add_clay with_wood 12 in
  with_both

let get_player_from_option (p : 'a option) : 'a =
  match p with Some x -> x | None -> empty_player

let player_with_settlement =
  get_player_from_option
    (fst (Test_Player.Player.build_settlment empty_player 3))
  |> Player.Player.add_clay

let player_with_road = Test_Player.Player.build_road empty_player 1

let player_with_two_roads =
  Test_Player.Player.build_road
    (get_player_from_option (fst player_with_road))
    2

let player_with_settlement =
  fst (Test_Player.Player.build_settlment empty_player 1)

let set_up_player =
  let built1s = get_player_from_option player_with_settlement in
  let built2s =
    get_player_from_option (fst (Player.Player.build_settlment built1s 2))
  in
  let built2s1r =
    get_player_from_option (fst (Player.Player.build_road built2s 1))
  in
  get_player_from_option (fst (Player.Player.build_road built2s1r 3))

module Test_Board = Game.Board

let initial_board = Test_Board.SmallBoard.initial_board

let board_with_settlement24 =
  Test_Board.SmallBoard.build_settlement initial_board 24

let board_with_settlement1 =
  Test_Board.SmallBoard.build_settlement initial_board 1

let board_with_road30 = Test_Board.SmallBoard.build_road initial_board 30

let board_with_settlement24 =
  Test_Board.SmallBoard.build_settlement initial_board 24

let board_with_settlement1 =
  Test_Board.SmallBoard.build_settlement initial_board 1

let board_with_road30 = Test_Board.SmallBoard.build_road initial_board 30
let board_with_road = Test_Board.SmallBoard.build_road initial_board 1

let get_player_from_option (p : 'a option) : 'a =
  match p with Some x -> x | None -> empty_player

let get_board_from_option (p : 'a option) : 'a =
  match p with Some x -> x | None -> initial_board

let board_with_settlement =
  Test_Board.SmallBoard.build_settlement initial_board 6

let board_with_city =
  Test_Board.SmallBoard.build_city
    (get_board_from_option board_with_settlement)
    6

let player_tests =
  [
    (* Player tests *)
    ( "empty player starts with 4 clay" >:: fun _ ->
      assert_equal 4 (Player.Player.get_clay empty_player) );
    ( "empty player starts with 2 wheat" >:: fun _ ->
      assert_equal 2 (Player.Player.get_wheat empty_player) );
    ( "empty player starts with 0 victory points" >:: fun _ ->
      assert_equal 0 (Player.Player.get_victory_points empty_player) );
    ( "empty player has no roads" >:: fun _ ->
      assert_equal [] (Player.Player.get_road_locations empty_player) );
    ( "empty player has no buildable_locs" >:: fun _ ->
      assert_equal [] (Player.Player.get_buildable_locs empty_player) );
    ( "add_resource add an ore" >:: fun _ ->
      assert_equal 1
        (Player.Player.get_ore (Player.Player.add_ore empty_player)) );
    ( "add_resource add a wood" >:: fun _ ->
      assert_equal 5
        (Player.Player.get_wood (Player.Player.add_wood empty_player)) );
    ( "build_road build a road and check that it exists" >:: fun _ ->
      assert_equal [ 2; 0 ] (Player.Player.get_road_locations set_up_player) );
    ( "build_road try to build a road and it's not a buildable loc" >:: fun _ ->
      assert_equal (None, 3) (Player.Player.build_road empty_player 9) );
    ( "build_road try to build a road and there aren't enough resources"
    >:: fun _ ->
      assert_equal (None, 2) (Player.Player.build_road set_up_player 1) );
    ( "build_settlement empty player builds their first settlement and it uses \
       a sheep"
    >:: fun _ ->
      assert_equal 1
        (Player.Player.get_sheep
           (get_player_from_option
              (fst (Player.Player.build_settlment empty_player 9)))) );
    ( "build_settlement empty player builds their first settlement and it exists"
    >:: fun _ ->
      assert_equal [ 9 ]
        (Player.Player.get_settlement_locations
           (get_player_from_option
              (fst (Player.Player.build_settlment empty_player 10)))) );
    ( "settlement_count empty players builds twice and has 3 settlements left"
    >:: fun _ ->
      assert_equal 3
        (Player.Player.get_settlement_count
           (get_player_from_option
              (fst
                 (Player.Player.build_settlment
                    (get_player_from_option player_with_settlement)
                    8)))) );
    ( "build_settlement at a non buildable loc" >:: fun _ ->
      assert_equal (None, 3)
        (Player.Player.build_settlment
           (get_player_from_option
              (fst
                 (Player.Player.build_settlment
                    (get_player_from_option player_with_settlement)
                    4)))
           12) );
    ( "build_city not enough resources" >:: fun _ ->
      assert_equal (None, 2) (Player.Player.build_city set_up_player 1) );
    ( "build_city legally" >:: fun _ ->
      assert_equal [ 0 ]
        (let with_ore = add_n_resources Player.Player.add_ore set_up_player 3 in
         let with_both = add_n_resources Player.Player.add_wheat with_ore 2 in

         Player.Player.get_city_locations
           (get_player_from_option (fst (Player.Player.build_city with_both 1))))
    );
    ( "Player loses three clay after trading clay" >:: fun _ ->
      assert_equal 4 (Player.Player.get_clay (Player.Player.trade_clay player_three_all)));
    ( "Player loses three wood after trading wood" >:: fun _ ->
      assert_equal 4 (Player.Player.get_wood (Player.Player.trade_wood player_three_all)));
    ( "Player loses three wheat after trading wheat" >:: fun _ ->
      assert_equal 2 (Player.Player.get_wheat (Player.Player.trade_wheat player_three_all)));
    ( "Player loses three sheep after trading sheep" >:: fun _ ->
      assert_equal 2 (Player.Player.get_sheep (Player.Player.trade_sheep player_three_all)));
    ( "Player loses three ore after trading ore" >:: fun _ ->
      assert_equal 0 (Player.Player.get_ore (Player.Player.trade_ore player_three_all)));
    ( "Player add sheep and then lose sheep" >:: fun _ ->
      assert_equal 3 (Player.Player.get_sheep (Player.Player.trade_sheep (Player.Player.add_sheep player_three_all))));
    ( "Player add ore and then lose ore" >:: fun _ ->
      assert_equal 1 (Player.Player.get_ore (Player.Player.trade_ore (Player.Player.add_ore player_three_all))));
    ( "Player add wheat and then lose wheat" >:: fun _ ->
      assert_equal 3 (Player.Player.get_wheat(Player.Player.trade_wheat (Player.Player.add_wheat player_three_all))));
    ( "Player add wood and then lose wood" >:: fun _ ->
      assert_equal 5 (Player.Player.get_wood (Player.Player.trade_wood (Player.Player.add_wood player_three_all))));
    ( "Player add clay and then lose clay" >:: fun _ ->
      assert_equal 5 (Player.Player.get_clay (Player.Player.trade_clay (Player.Player.add_clay player_three_all))));

    ( "Player trade sheep and then add sheep" >:: fun _ ->
      assert_equal 3 (Player.Player.get_sheep (Player.Player.add_sheep (Player.Player.trade_sheep player_three_all))));
    ( "Player trad ore and then add ore" >:: fun _ ->
      assert_equal 1 (Player.Player.get_ore (Player.Player.add_ore (Player.Player.trade_ore player_three_all))));
    ( "Player trade wheat and then add wheat" >:: fun _ ->
      assert_equal 3 (Player.Player.get_wheat(Player.Player.add_wheat (Player.Player.trade_wheat player_three_all))));
    ( "Player trade wood and then add wood" >:: fun _ ->
      assert_equal 5 (Player.Player.get_wood (Player.Player.add_wood (Player.Player.trade_wood player_three_all))));
    ( "Player trade clay and then add clay" >:: fun _ ->
      assert_equal 5 (Player.Player.get_clay (Player.Player.add_clay (Player.Player.trade_clay player_three_all))));

    ( "Player trade and then add different resources" >:: fun _ ->
      assert_equal 2 (Player.Player.get_sheep (Player.Player.add_ore (Player.Player.trade_sheep player_three_all))));
    ( "Player add and then trade different resources" >:: fun _ ->
      assert_equal 0 (Player.Player.get_ore (Player.Player.add_sheep (Player.Player.trade_ore player_three_all))));
    
  ]

let board_tests =
  [
    ( "initial board has 24 nodes" >:: fun _ ->
      assert_equal 24
        (List.length (Board.SmallBoard.get_node_lst initial_board)) );
    ( "initial board has 30 roads" >:: fun _ ->
      assert_equal 30
        (List.length (Board.SmallBoard.get_edge_lst initial_board)) );
    ( "board after building has 24 nodes" >:: fun _ ->
      assert_equal 24
        (List.length
           (Board.SmallBoard.get_node_lst
              (get_board_from_option board_with_settlement))) );
    ( "board after building has 30 roads" >:: fun _ ->
      assert_equal 30
        (List.length
           (Board.SmallBoard.get_edge_lst
              (get_board_from_option board_with_settlement))) );
    ( "initial board has no cities" >:: fun _ ->
      assert_equal false
        (List.nth (Board.SmallBoard.get_node_lst initial_board) 23).is_city );
    ( "initial board has no settlements" >:: fun _ ->
      assert_equal false
        (List.nth (Board.SmallBoard.get_node_lst initial_board) 0).is_settlement
    );
    ( "initial board has no roads" >:: fun _ ->
      assert_equal false
        (List.nth (Board.SmallBoard.get_edge_lst initial_board) 29).is_road );
    ( "build_settlement at 1" >:: fun _ ->
      assert_equal true
        (List.nth
           (Board.SmallBoard.get_node_lst
              (get_board_from_option board_with_settlement1))
           0)
          .is_settlement );
    ( "build_settlement at 24" >:: fun _ ->
      assert_equal true
        (List.nth
           (Board.SmallBoard.get_node_lst
              (get_board_from_option board_with_settlement24))
           23)
          .is_settlement );
    ( "build_road build road 30 and confirm that it exists" >:: fun _ ->
      assert_equal true
        (List.nth
           (Board.SmallBoard.get_edge_lst
              (get_board_from_option board_with_road30))
           29)
          .is_road );
    ( "build_road build a road and confirm that it exists" >:: fun _ ->
      assert_equal true
        (List.nth
           (Board.SmallBoard.get_edge_lst
              (get_board_from_option
                 (Board.SmallBoard.build_road initial_board 9)))
           8)
          .is_road );
    ( "build_road where one already exists" >:: fun _ ->
      assert_equal None
        (Board.SmallBoard.build_road (get_board_from_option board_with_road) 1)
    );
    ( "build_settlement where one already exists" >:: fun _ ->
      assert_equal None
        (Board.SmallBoard.build_settlement
           (get_board_from_option board_with_settlement)
           6) );
    ( "build_settlement where there is a city" >:: fun _ ->
      assert_equal None
        (Board.SmallBoard.build_settlement
           (get_board_from_option board_with_city)
           6) );
    ( "build_settlement at a legal location" >:: fun _ ->
      assert_equal true
        (List.nth
           (Board.SmallBoard.get_node_lst
              (get_board_from_option
                 (Board.SmallBoard.build_settlement initial_board 13)))
           12)
          .is_settlement );
    ( "build_city where one already exists" >:: fun _ ->
      assert_equal None
        (Board.SmallBoard.build_city (get_board_from_option board_with_city) 6)
    );
    ( "build_city at a legal location" >:: fun _ ->
      assert_equal true
        (List.nth
           (Board.SmallBoard.get_node_lst
              (get_board_from_option
                 (Board.SmallBoard.build_city
                    (get_board_from_option board_with_settlement)
                    6)))
           5)
          .is_city );
  ]

let suite = "test suite for A2" >::: List.flatten [ player_tests; board_tests ]
let () = run_test_tt_main suite
