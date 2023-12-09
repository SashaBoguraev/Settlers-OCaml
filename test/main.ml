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

let board_with_settlement24 = Test_Board.SmallBoard.build_settlement initial_board 24

let board_with_settlement1 = Test_Board.SmallBoard.build_settlement initial_board 1 

let board_with_road30 = Test_Board.SmallBoard.build_road initial_board 30

let board_with_settlement24 = Test_Board.SmallBoard.build_settlement initial_board 24

let board_with_settlement1 = Test_Board.SmallBoard.build_settlement initial_board 1 

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
  ]

let board_tests =
  [
    ("initial board has 24 nodes" >::  fun _ -> assert_equal 24 (List.length (Board.SmallBoard.get_node_lst initial_board)));

    ("initial board has 30 roads" >::  fun _ -> assert_equal 30 (List.length (Board.SmallBoard.get_edge_lst initial_board)));

    ("board after building has 24 nodes" >::  fun _ -> assert_equal 24 (List.length (Board.SmallBoard.get_node_lst (get_board_from_option board_with_settlement))));

    ("board after building has 30 roads" >::  fun _ -> assert_equal 30 (List.length (Board.SmallBoard.get_edge_lst (get_board_from_option board_with_settlement))));
    ( "initial board has no cities" >:: fun _ ->
      assert_equal false
        (List.nth
           (Board.SmallBoard.get_node_lst
              (initial_board
              ))
           23)
          .is_city );
    ( "initial board has no settlements" >:: fun _ ->
      assert_equal false
        (List.nth
           (Board.SmallBoard.get_node_lst
              (initial_board
              ))
           0)
          .is_settlement );
    ( "initial board has no roads" >:: fun _ ->
      assert_equal false
        (List.nth
           (Board.SmallBoard.get_edge_lst
              (initial_board
              ))
           29)
          .is_road );
    ( "build_settlement at 1" >:: fun _ ->
      assert_equal true
        (List.nth
           (Board.SmallBoard.get_node_lst
              (get_board_from_option
                 (board_with_settlement1)))
           0)
          .is_settlement );
    ( "build_settlement at 24" >:: fun _ ->
      assert_equal true
        (List.nth
           (Board.SmallBoard.get_node_lst
              (get_board_from_option
                 (board_with_settlement24)))
           23)
          .is_settlement );
    ( "build_road build road 30 and confirm that it exists" >:: fun _ ->
      assert_equal true
        (List.nth
           (Board.SmallBoard.get_edge_lst
              (get_board_from_option
                 (board_with_road30)))
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