open Player

type resource = Clay | Wood | Sheep | Ore | Wheat

type node = {
  border_one_resource : resource option;
  border_one_number : int option;
  border_two_resource : resource option;
  border_two_number : int option;
  border_three_resource : resource option;
  border_three_number : int option;
  is_settlement : bool;
  is_city : bool;
}

type edge = { node1 : node; node2 : node; is_road : bool }

module type BoardType = sig
  type t

  val build_vertices : node list -> node option list list
  val edges : edge list
  val nodes : node list
  val initial_board : t
  val build_road : t -> int -> t option
  val build_settlement : t -> int -> t option
  val build_city : t -> int -> t option
  val get_adj_lst : t -> node option list list
  val get_node_lst : t -> node list
  val get_edge_lst : t -> edge list
  val get_node_border_one_resource : node -> resource option
  val get_node_border_one_number : node -> int option
  val get_node_border_two_resource : node -> resource option
  val get_node_border_two_number : node -> int option
  val get_node_border_three_resource : node -> resource option
  val get_node_border_three_number : node -> int option
end

module SmallBoard : BoardType = struct
  type t = {
    adj_lst : node option list list;
    node_lst : node list;
    edge_lst : edge list;
  }

  let get_adj_lst board = board.adj_lst
  let get_node_lst board = board.node_lst
  let get_edge_lst board = board.edge_lst
  let get_node_border_one_resource node = node.border_one_resource
  let get_node_border_one_number node = node.border_one_number
  let get_node_border_two_resource node = node.border_two_resource
  let get_node_border_two_number node = node.border_two_number
  let get_node_border_three_resource node = node.border_three_resource
  let get_node_border_three_number node = node.border_three_number

  let node_one : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = Some Sheep;
      border_three_number = Some 6;
      is_settlement = false;
      is_city = false;
    }

  let node_two : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = Some Wood;
      border_three_number = Some 4;
      is_settlement = false;
      is_city = false;
    }

  let node_three : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = Some Sheep;
      border_three_number = Some 6;
      is_settlement = false;
      is_city = false;
    }

  let node_four : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = Some Sheep;
      border_two_number = Some 6;
      border_three_resource = Some Wood;
      border_three_number = Some 4;
      is_settlement = false;
      is_city = false;
    }

  let node_five : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = Some Wood;
      border_two_number = Some 4;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let node_six : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = Some Sheep;
      border_two_number = Some 6;
      border_three_resource = Some Clay;
      border_three_number = Some 3;
      is_settlement = false;
      is_city = false;
    }

  let node_seven : node =
    {
      border_one_resource = Some Sheep;
      border_one_number = Some 6;
      border_two_resource = Some Wood;
      border_two_number = Some 4;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let node_eight : node =
    {
      border_one_resource = Some Wood;
      border_one_number = Some 4;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = Some Wood;
      border_three_number = Some 2;
      is_settlement = false;
      is_city = false;
    }

  let node_nine : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = Some Clay;
      border_three_number = Some 3;
      is_settlement = false;
      is_city = false;
    }

  let node_ten : node =
    {
      border_one_resource = Some Sheep;
      border_one_number = Some 6;
      border_two_resource = Some Clay;
      border_two_number = Some 10;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let node_eleven : node =
    {
      border_one_resource = Some Wood;
      border_one_number = Some 4;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = Some Wood;
      border_three_number = Some 2;
      is_settlement = false;
      is_city = false;
    }

  let node_twelve : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = Some Wood;
      border_two_number = Some 2;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let node_thirteen : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = Some Clay;
      border_two_number = Some 3;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let node_fourteen : node =
    {
      border_one_resource = Some Clay;
      border_one_number = Some 3;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = Some Wheat;
      border_three_number = Some 5;
      is_settlement = false;
      is_city = false;
    }

  let node_fifteen : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = Some Wood;
      border_two_number = Some 2;
      border_three_resource = Some Ore;
      border_three_number = Some 1;
      is_settlement = false;
      is_city = false;
    }

  let node_sixteen : node =
    {
      border_one_resource = Some Wood;
      border_one_number = Some 2;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let node_seventeen : node =
    {
      border_one_resource = Some Clay;
      border_one_number = Some 3;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = Some Sheep;
      border_three_number = Some 6;
      is_settlement = false;
      is_city = false;
    }

  let node_eighteen : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = Some Wheat;
      border_two_number = Some 5;
      border_three_resource = Some Ore;
      border_three_number = Some 1;
      is_settlement = false;
      is_city = false;
    }

  let node_nineteen : node =
    {
      border_one_resource = Some Wood;
      border_one_number = Some 2;
      border_two_resource = Some Ore;
      border_two_number = Some 1;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let node_twenty : node =
    {
      border_one_resource = None;
      border_one_number = None;
      border_two_resource = Some Wheat;
      border_two_number = Some 5;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let node_twentyone : node =
    {
      border_one_resource = Some Wheat;
      border_one_number = Some 5;
      border_two_resource = Some Ore;
      border_two_number = Some 1;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let node_twentytwo : node =
    {
      border_one_resource = Some Ore;
      border_one_number = Some 1;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let node_twentythree : node =
    {
      border_one_resource = Some Wheat;
      border_one_number = Some 5;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let node_twentyfour : node =
    {
      border_one_resource = Some Ore;
      border_one_number = Some 1;
      border_two_resource = None;
      border_two_number = None;
      border_three_resource = None;
      border_three_number = None;
      is_settlement = false;
      is_city = false;
    }

  let nodes =
    [
      node_one;
      node_two;
      node_three;
      node_four;
      node_five;
      node_six;
      node_seven;
      node_eight;
      node_nine;
      node_ten;
      node_eleven;
      node_twelve;
      node_thirteen;
      node_fourteen;
      node_fifteen;
      node_sixteen;
      node_seventeen;
      node_eighteen;
      node_nineteen;
      node_twenty;
      node_twentyone;
      node_twentytwo;
      node_twentythree;
      node_twentyfour;
    ]

  let build_vertices (all_nodes : node list) : node option list list =
    [
      [ None; Some (List.nth all_nodes 2); Some (List.nth all_nodes 3) ];
      [ None; Some (List.nth all_nodes 5); Some (List.nth all_nodes 4) ];
      [ None; Some (List.nth all_nodes 0); Some (List.nth all_nodes 5) ];
      [
        Some (List.nth all_nodes 0);
        Some (List.nth all_nodes 1);
        Some (List.nth all_nodes 6);
      ];
      [ Some (List.nth all_nodes 1); None; Some (List.nth all_nodes 7) ];
      [
        Some (List.nth all_nodes 2);
        Some (List.nth all_nodes 8);
        Some (List.nth all_nodes 15);
      ];
      [
        Some (List.nth all_nodes 3);
        Some (List.nth all_nodes 9);
        Some (List.nth all_nodes 10);
      ];
      [
        Some (List.nth all_nodes 4);
        Some (List.nth all_nodes 10);
        Some (List.nth all_nodes 11);
      ];
      [ None; Some (List.nth all_nodes 5); Some (List.nth all_nodes 12) ];
      [
        Some (List.nth all_nodes 5);
        Some (List.nth all_nodes 6);
        Some (List.nth all_nodes 13);
      ];
      [
        Some (List.nth all_nodes 6);
        Some (List.nth all_nodes 7);
        Some (List.nth all_nodes 14);
      ];
      [ Some (List.nth all_nodes 7); None; Some (List.nth all_nodes 15) ];
      [ Some (List.nth all_nodes 8); None; Some (List.nth all_nodes 16) ];
      [
        Some (List.nth all_nodes 9);
        Some (List.nth all_nodes 16);
        Some (List.nth all_nodes 17);
      ];
      [
        Some (List.nth all_nodes 10);
        Some (List.nth all_nodes 17);
        Some (List.nth all_nodes 18);
      ];
      [ Some (List.nth all_nodes 11); Some (List.nth all_nodes 15); None ];
      [
        Some (List.nth all_nodes 12);
        Some (List.nth all_nodes 13);
        Some (List.nth all_nodes 19);
      ];
      [
        Some (List.nth all_nodes 13);
        Some (List.nth all_nodes 14);
        Some (List.nth all_nodes 20);
      ];
      [
        Some (List.nth all_nodes 14);
        Some (List.nth all_nodes 15);
        Some (List.nth all_nodes 21);
      ];
      [ Some (List.nth all_nodes 16); None; Some (List.nth all_nodes 22) ];
      [
        Some (List.nth all_nodes 17);
        Some (List.nth all_nodes 22);
        Some (List.nth all_nodes 23);
      ];
      [ Some (List.nth all_nodes 18); Some (List.nth all_nodes 23); None ];
      [ Some (List.nth all_nodes 20); Some (List.nth all_nodes 22); None ];
      [ Some (List.nth all_nodes 20); Some (List.nth all_nodes 21); None ];
    ]

  let edges =
    [
      { node1 = node_one; node2 = node_three; is_road = false };
      { node1 = node_one; node2 = node_four; is_road = false };
      { node1 = node_two; node2 = node_four; is_road = false };
      { node1 = node_two; node2 = node_five; is_road = false };
      { node1 = node_three; node2 = node_six; is_road = false };
      { node1 = node_four; node2 = node_seven; is_road = false };
      { node1 = node_five; node2 = node_eight; is_road = false };
      { node1 = node_six; node2 = node_nine; is_road = false };
      { node1 = node_six; node2 = node_ten; is_road = false };
      { node1 = node_seven; node2 = node_ten; is_road = false };
      { node1 = node_seven; node2 = node_eleven; is_road = false };
      { node1 = node_eight; node2 = node_eleven; is_road = false };
      { node1 = node_eight; node2 = node_twelve; is_road = false };
      { node1 = node_nine; node2 = node_thirteen; is_road = false };
      { node1 = node_ten; node2 = node_fourteen; is_road = false };
      { node1 = node_eleven; node2 = node_fifteen; is_road = false };
      { node1 = node_twelve; node2 = node_sixteen; is_road = false };
      { node1 = node_thirteen; node2 = node_seventeen; is_road = false };
      { node1 = node_fourteen; node2 = node_seventeen; is_road = false };
      { node1 = node_fourteen; node2 = node_eighteen; is_road = false };
      { node1 = node_fifteen; node2 = node_eighteen; is_road = false };
      { node1 = node_fifteen; node2 = node_nineteen; is_road = false };
      { node1 = node_sixteen; node2 = node_nineteen; is_road = false };
      { node1 = node_seventeen; node2 = node_twenty; is_road = false };
      { node1 = node_eighteen; node2 = node_twentyone; is_road = false };
      { node1 = node_nineteen; node2 = node_twentytwo; is_road = false };
      { node1 = node_twenty; node2 = node_twentythree; is_road = false };
      { node1 = node_twentyone; node2 = node_twentythree; is_road = false };
      { node1 = node_twentyone; node2 = node_twentyfour; is_road = false };
      { node1 = node_twentytwo; node2 = node_twentyfour; is_road = false };
    ]

  let initial_board =
    { adj_lst = build_vertices nodes; node_lst = nodes; edge_lst = edges }

  (** Builds a road on the map at edge road_loc, returns the updated board. 
      If the move is illegal, it will return None (a move is illegal if there is already a piece at the desired location) *)
  let build_road (board : t) (road_loc : int) : t option =
    if (List.nth board.edge_lst (road_loc - 1)).is_road then None
    else
      let new_edges =
        List.mapi
          (fun i el ->
            if i = road_loc - 1 then { el with is_road = true } else el)
          board.edge_lst
      in
      Some
        { adj_lst = get_adj_lst board; node_lst = board.node_lst; edge_lst = new_edges }

  (** Builds a settlement on the map at vertex settlement_loc, returns the updated board. 
    If the move is illegal, it will return None (a move is illegal if there is already a piece at the desired location) *)
  let build_settlement (board : t) (settlement_loc : int) : t option =
    if
      (List.nth board.node_lst (settlement_loc - 1)).is_settlement
      || (List.nth board.node_lst (settlement_loc - 1)).is_city
    then None
    else
      let new_nodes =
        List.mapi
          (fun i el ->
            if i = settlement_loc - 1 then { el with is_settlement = true }
            else el)
          board.node_lst
      in
      Some
        {
          adj_lst = build_vertices new_nodes;
          node_lst = new_nodes;
          edge_lst = board.edge_lst;
        }

  (** Builds a city on the map at vertex city_loc, returns the updated board. 
  If the move is illegal, it will return None (a move is illegal if there is already a piece at the desired location) *)
  let build_city (board : t) (city_loc : int) : t option =
    if
      ((*List.nth board.node_lst (city_loc - 1)).is_settlement
      && *)not (List.nth board.node_lst (city_loc - 1)).is_city)
    then
      let new_nodes =
        List.mapi
          (fun i el ->
            if i = city_loc - 1 then
              { el with is_city = true; is_settlement = false }
            else el)
          board.node_lst
      in
      Some
        {
          adj_lst = build_vertices new_nodes;
          node_lst = new_nodes;
          edge_lst = board.edge_lst;
        }
    else None
end
