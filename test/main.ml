open OUnit2
open Game

(********************************************************************
   Here are some helper functions for testing players and boards
 ********************************************************************)
module Test_Player = Game.Player.Player

let empty_player = Test_Player.empty

let player_tests =
  [
    (* Player tests *)
    ( "empty player starts with 2 of each resource" >:: fun _ ->
      assert_equal 2 (Player.Player.get_clay empty_player) );
    ( "empty player starts with 0 victory points" >:: fun _ ->
      assert_equal 2 (Player.Player.get_clay empty_player) );
  ]

let suite = "test suite for A2" >::: List.flatten [ player_tests ]
let () = run_test_tt_main suite
