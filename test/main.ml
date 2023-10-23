open OUnit2
open Game

(********************************************************************
   Here are some helper functions for testing players and boards
 ********************************************************************)
module Test_Player = Game.Player

let empty_player = Test_Player.Player.empty
let player_with_road = fst (Test_Player.Player.build_road empty_player 1)

let player_with_two_roads =
  fst
    (Test_Player.Player.build_road
       (match player_with_road with Some x -> x | None -> empty_player)
       1)

module Test_Board = Game.Board

let initial_board = Test_Board.SmallBoard.initial_board
let board_with_road = Test_Board.SmallBoard.build_road initial_board 1

let board_with_settlement =
  Test_Board.SmallBoard.build_settlement initial_board 6

let get_player_from_option (p : 'a option) : 'a =
  match p with Some x -> x | None -> empty_player

let get_board_from_option (p : 'a option) : 'a =
  match p with Some x -> x | None -> initial_board

let player_tests =
  [
    (* Player tests *)
    ( "empty player starts with 2 of each resource" >:: fun _ ->
      assert_equal 2 (Player.Player.get_clay empty_player) );
    ( "empty player starts with 0 victory points" >:: fun _ ->
      assert_equal 0 (Player.Player.get_victory_points empty_player) );
    ( "empty player has no roads" >:: fun _ ->
      assert_equal [] (Player.Player.get_road_locations empty_player) );
    ( "add_resource add an ore" >:: fun _ ->
      assert_equal 1
        (Player.Player.get_ore (Player.Player.add_ore empty_player)) );
    ( "add_resource add a wood" >:: fun _ ->
      assert_equal 3
        (Player.Player.get_wood (Player.Player.add_wood empty_player)) );
    ( "build_road build a road and check that it exists" >:: fun _ ->
      assert_equal [ 0 ]
        (Player.Player.get_road_locations
           (get_player_from_option player_with_road)) );
    ( "build_road try to build a road and it doesn't work" >:: fun _ ->
      assert_equal None
        (match
           fst
             (Player.Player.build_road
                (get_player_from_option player_with_two_roads)
                1)
         with
        | Some x -> Some x
        | None -> None) );
  ]

let board_tests =
  [
    ( "build_road build a road and confirm that it exists" >:: fun _ ->
      assert_equal true
        (List.nth
           (Board.SmallBoard.get_edge_lst
              (get_board_from_option board_with_road))
           0)
          .is_road );
    ( "build_city that isn't legal" >:: fun _ ->
      assert_equal None (Board.SmallBoard.build_city initial_board 1) );
    ( "build_city that is legal" >:: fun _ ->
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
