open Player

type resource = Clay | Wood | Sheep | Ore | Wheat

<<<<<<< HEAD
type node = {border_one_resource: resource option; border_one_number: int option; border_two_resource: resource option; border_two_number: int option; border_three_resource: resource option; border_three_number: int option; is_settlement: bool; is_city: bool}
type edge = {node1: node; node2: node; is_road: bool}
=======
type node = {border_one_resource: resource option; border_one_number: int option; border_two_resource: resource option; border_two_number: int option; border_three_resource: resource option; border_three_number: int option; is_settlement: bool; is_city: bool; player: Player.t option}
type edge = {node1: node; node2: node; is_road: bool; player: Player.t option}
>>>>>>> main

module type BoardType = sig
  type t

<<<<<<< HEAD
  val build_vertices: node list -> node option list list
  val edges: edge list
  val nodes: node list
=======
  val initial_vertices: node option list list
  val initial_edges: edge list
>>>>>>> main

  val initial_board: t

  val build_road : t -> int -> t option

  val build_settlement : t -> int -> t option

<<<<<<< HEAD
  val build_city : t -> int -> t option
=======
  val build_city : t -> t *)
>>>>>>> main
end

module SmallBoard: BoardType = struct
  type t = node option list list * edge list
  
  let node_one: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Sheep; border_three_number = Some 6;
                        is_settlement = false; is_city = false}
  
  let node_two: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Wood; border_three_number = Some 4;
                        is_settlement = false; is_city = false}
  
  let node_three: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Sheep; border_three_number = Some 6;
                        is_settlement = false; is_city = false}

  let node_four: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Sheep; border_two_number = Some 6;
                        border_three_resource = Some Wood; border_three_number = Some 4;
                        is_settlement = false; is_city = false}
  
  let node_five: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Wood; border_two_number = Some 4;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}
  
  let node_six: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Sheep; border_two_number = Some 6;
                        border_three_resource = Some Clay; border_three_number = Some 3;
                        is_settlement = false; is_city = false}
  
  let node_seven: node = {border_one_resource = Some Sheep; border_one_number = Some 6;
                        border_two_resource = Some Wood; border_two_number = Some 4;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}
  
  let node_eight: node = {border_one_resource = Some Wood; border_one_number = Some 4;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Wood; border_three_number = Some 2;
                        is_settlement = false; is_city = false}
  
  let node_nine: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Clay; border_three_number = Some 3;
                        is_settlement = false; is_city = false}

  let node_ten: node = {border_one_resource = Some Sheep; border_one_number = Some 6;
                        border_two_resource = Some Clay; border_two_number = Some 10;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}
  
  let node_eleven: node = {border_one_resource = Some Wood; border_one_number = Some 4;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Wood; border_three_number = Some 2;
                        is_settlement = false; is_city = false}
  
  let node_twelve: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Wood; border_two_number = Some 2;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}

  let node_thirteen: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Clay; border_two_number = Some 3;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}
  
  let node_fourteen: node = {border_one_resource = Some Clay; border_one_number = Some 3;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Wheat; border_three_number = Some 5;
                        is_settlement = false; is_city = false}
  
  let node_fifteen: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Wood; border_two_number = Some 2;
                        border_three_resource = Some Ore; border_three_number = Some 1;
                        is_settlement = false; is_city = false}

  let node_sixteen: node = {border_one_resource = Some Wood; border_one_number = Some 2;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}
  
  let node_seventeen: node = {border_one_resource = Some Clay; border_one_number = Some 3;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Sheep; border_three_number = Some 6;
                        is_settlement = false; is_city = false}
  
  let node_eighteen: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Wheat; border_two_number = Some 5;
                        border_three_resource = Some Ore; border_three_number = Some 1;
                        is_settlement = false; is_city = false}
  
  let node_nineteen: node = {border_one_resource = Some Wood; border_one_number = Some 2;
                        border_two_resource = Some Ore; border_two_number = Some 1;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}
  
  let node_twenty: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Wheat; border_two_number = Some 5;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}
  
  let node_twentyone: node = {border_one_resource = Some Wheat; border_one_number = Some 5;
                        border_two_resource = Some Ore; border_two_number = Some 1;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}

  let node_twentytwo: node = {border_one_resource = Some Ore; border_one_number = Some 1;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}
  
  let node_twentythree: node = {border_one_resource = Some Wheat; border_one_number = Some 5;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}
  
  let node_twentyfour: node = {border_one_resource = Some Ore; border_one_number = Some 1;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = None; border_three_number = None;
                        is_settlement = false; is_city = false}

<<<<<<< HEAD
  let nodes = [
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
    node_twentyfour
  ]

  let build_vertices (all_nodes: node list): node option list list = [
    [None; Some (List.nth all_nodes 2); Some (List.nth all_nodes 3)];
    [None; Some (List.nth all_nodes 5); Some (List.nth all_nodes 4)];
    [None; Some (List.nth all_nodes 0); Some (List.nth all_nodes 5)];
    [Some (List.nth all_nodes 0); Some (List.nth all_nodes 1); Some (List.nth all_nodes 6)];
    [Some (List.nth all_nodes 1); None; Some (List.nth all_nodes 7)];
    [Some (List.nth all_nodes 2); Some (List.nth all_nodes 8); Some (List.nth all_nodes 15)];
    [Some (List.nth all_nodes 3); Some (List.nth all_nodes 9); Some (List.nth all_nodes 10)];
    [Some (List.nth all_nodes 4); Some (List.nth all_nodes 10); Some (List.nth all_nodes 11)];
    [None; Some (List.nth all_nodes 5); Some (List.nth all_nodes 12)];
    [Some (List.nth all_nodes 5); Some (List.nth all_nodes 6); Some (List.nth all_nodes 13)];
    [Some (List.nth all_nodes 6); Some (List.nth all_nodes 7); Some (List.nth all_nodes 14)];
    [Some (List.nth all_nodes 7); None; Some (List.nth all_nodes 15)];
    [Some (List.nth all_nodes 8); None; Some (List.nth all_nodes 16)];
    [Some (List.nth all_nodes 9); Some (List.nth all_nodes 16); Some (List.nth all_nodes 17)];
    [Some (List.nth all_nodes 10); Some (List.nth all_nodes 17); Some (List.nth all_nodes 18)];
    [Some (List.nth all_nodes 11); Some (List.nth all_nodes 15); None];
    [Some (List.nth all_nodes 12); Some (List.nth all_nodes 13); Some (List.nth all_nodes 19)];
    [Some (List.nth all_nodes 13); Some (List.nth all_nodes 14); Some (List.nth all_nodes 20)];
    [Some (List.nth all_nodes 14); Some (List.nth all_nodes 15); Some (List.nth all_nodes 21)];
    [Some (List.nth all_nodes 16); None; Some (List.nth all_nodes 22)];
    [Some (List.nth all_nodes 17); Some (List.nth all_nodes 22); Some (List.nth all_nodes 23)];
    [Some (List.nth all_nodes 18); Some (List.nth all_nodes 23); None];
    [Some (List.nth all_nodes 20); Some (List.nth all_nodes 22); None];
    [Some (List.nth all_nodes 20); Some (List.nth all_nodes 21); None]
=======
  let initial_vertices = [
    [None; Some node_three; Some node_four];
    [None; Some node_four; Some node_five];
    [None; Some node_one; Some node_six];
    [Some node_one; Some node_two; Some node_seven];
    [Some node_two; None; Some node_eight];
    [Some node_three; Some node_nine; Some node_sixteen];
    [Some node_four; Some node_ten; Some node_eleven];
    [Some node_five; Some node_eleven; Some node_twelve];
    [None; Some node_six; Some node_thirteen];
    [Some node_six; Some node_seven; Some node_fourteen];
    [Some node_seven; Some node_eight; Some node_fifteen];
    [Some node_eight; None; Some node_sixteen];
    [Some node_nine; None; Some node_seventeen];
    [Some node_ten; Some node_seventeen; Some node_eighteen];
    [Some node_eleven; Some node_eighteen; Some node_nineteen];
    [Some node_twelve; Some node_sixteen; None];
    [Some node_thirteen; Some node_fourteen; Some node_twenty];
    [Some node_fourteen; Some node_fifteen; Some node_twentyone];
    [Some node_fifteen; Some node_sixteen; Some node_twentytwo];
    [Some node_seventeen; None; Some node_twentythree];
    [Some node_eighteen; Some node_twentythree; Some node_twentyfour];
    [Some node_nineteen; Some node_twentyfour; None];
    [Some node_twentyone; Some node_twentythree; None];
    [Some node_twentyone; Some node_twentytwo; None]
  ]

  let initial_edges = [
    {node1 = node_one; node2 = node_three; is_road = false; player = None};
    {node1 = node_one; node2 = node_four; is_road = false; player = None};
    {node1 = node_two; node2 = node_four; is_road = false; player = None};
    {node1 = node_two; node2 = node_five; is_road = false; player = None};
    {node1 = node_three; node2 = node_six; is_road = false; player = None};
    {node1 = node_four; node2 = node_seven; is_road = false; player = None};
    {node1 = node_five; node2 = node_eight; is_road = false; player = None};
    {node1 = node_six; node2 = node_nine; is_road = false; player = None};
    {node1 = node_six; node2 = node_ten; is_road = false; player = None};
    {node1 = node_seven; node2 = node_ten; is_road = false; player = None};
    {node1 = node_seven; node2 = node_eleven; is_road = false; player = None};
    {node1 = node_eight; node2 = node_eleven; is_road = false; player = None};
    {node1 = node_eight; node2 = node_twelve; is_road = false; player = None};
    {node1 = node_nine; node2 = node_thirteen; is_road = false; player = None};
    {node1 = node_ten; node2 = node_fourteen; is_road = false; player = None};
    {node1 = node_eleven; node2 = node_fifteen; is_road = false; player = None};
    {node1 = node_twelve; node2 = node_sixteen; is_road = false; player = None};
    {node1 = node_thirteen; node2 = node_seventeen; is_road = false; player = None};
    {node1 = node_fourteen; node2 = node_seventeen; is_road = false; player = None};
    {node1 = node_fourteen; node2 = node_eighteen; is_road = false; player = None};
    {node1 = node_fifteen; node2 = node_eighteen; is_road = false; player = None};
    {node1 = node_fifteen; node2 = node_nineteen; is_road = false; player = None};
    {node1 = node_seventeen; node2 = node_twenty; is_road = false; player = None};
    {node1 = node_eighteen; node2 = node_twentyone; is_road = false; player = None};
    {node1 = node_nineteen; node2 = node_twentytwo; is_road = false; player = None};
    {node1 = node_twenty; node2 = node_twentythree; is_road = false; player = None};
    {node1 = node_twentyone; node2 = node_twentythree; is_road = false; player = None};
    {node1 = node_twentyone; node2 = node_twentyfour; is_road = false; player = None};
    {node1 = node_twentytwo; node2 = node_twentyfour; is_road = false; player = None};
>>>>>>> main
  ]

  let edges = [
    {node1 = node_one; node2 = node_three; is_road = false};
    {node1 = node_one; node2 = node_four; is_road = false};
    {node1 = node_two; node2 = node_four; is_road = false};
    {node1 = node_two; node2 = node_five; is_road = false};
    {node1 = node_three; node2 = node_six; is_road = false; };
    {node1 = node_four; node2 = node_seven; is_road = false; };
    {node1 = node_five; node2 = node_eight; is_road = false; };
    {node1 = node_six; node2 = node_nine; is_road = false; };
    {node1 = node_six; node2 = node_ten; is_road = false; };
    {node1 = node_seven; node2 = node_ten; is_road = false; };
    {node1 = node_seven; node2 = node_eleven; is_road = false; };
    {node1 = node_eight; node2 = node_eleven; is_road = false; };
    {node1 = node_eight; node2 = node_twelve; is_road = false; };
    {node1 = node_nine; node2 = node_thirteen; is_road = false; };
    {node1 = node_ten; node2 = node_fourteen; is_road = false; };
    {node1 = node_eleven; node2 = node_fifteen; is_road = false; };
    {node1 = node_twelve; node2 = node_sixteen; is_road = false;};
    {node1 = node_thirteen; node2 = node_seventeen; is_road = false;};
    {node1 = node_fourteen; node2 = node_seventeen; is_road = false;};
    {node1 = node_fourteen; node2 = node_eighteen; is_road = false;};
    {node1 = node_fifteen; node2 = node_eighteen; is_road = false;};
    {node1 = node_fifteen; node2 = node_nineteen; is_road = false;};
    {node1 = node_seventeen; node2 = node_twenty; is_road = false;};
    {node1 = node_eighteen; node2 = node_twentyone; is_road = false;};
    {node1 = node_nineteen; node2 = node_twentytwo; is_road = false;};
    {node1 = node_twenty; node2 = node_twentythree; is_road = false};
    {node1 = node_twentyone; node2 = node_twentythree; is_road = false; };
    {node1 = node_twentyone; node2 = node_twentyfour; is_road = false;};
    {node1 = node_twentytwo; node2 = node_twentyfour; is_road = false};
  ]

<<<<<<< HEAD
  let initial_board = (build_vertices nodes, edges)


  (** Builds a road on the map at edge road_loc, returns the updated board. 
      If the move is illegal, it will return None (a move is illegal if there is already a piece at the desired location) *)
  let build_road (board: t) (road_loc: int): t option = 
    if not (List.nth edges (road_loc-1)).is_road then None
    else let new_edges = List.mapi (fun i el -> if i = (road_loc-1) then {el with is_road=true} else el) edges
  in Some (build_vertices nodes, new_edges)

  (** Builds a settlement on the map at vertex settlement_loc, returns the updated board. 
    If the move is illegal, it will return None (a move is illegal if there is already a piece at the desired location) *)
  let build_settlement (board: t) (settlement_loc: int): t option = 
    if not ((List.nth nodes (settlement_loc-1)).is_settlement || (List.nth nodes (settlement_loc-1)).is_city) then None
    else let new_nodes = List.mapi (fun i el -> if i = (settlement_loc-1) then {el with is_settlement=true} else el) nodes
  in Some (build_vertices new_nodes, edges)

  (** Builds a city on the map at vertex city_loc, returns the updated board. 
  If the move is illegal, it will return None (a move is illegal if there is already a piece at the desired location) *)
  let build_city (board: t) (city_loc: int): t option = 
    if (List.nth nodes (city_loc-1)).is_settlement && not (List.nth nodes (city_loc-1)).is_city then None
    else let new_nodes = List.mapi (fun i el -> if i = (city_loc-1) then {el with is_city=true; is_settlement = false} else el) nodes
  in Some (build_vertices new_nodes, edges)

=======
>>>>>>> main
end
