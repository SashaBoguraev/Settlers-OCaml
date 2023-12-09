open Player

(** The resources used in the game *)
type resource = Clay | Wood | Sheep | Ore | Wheat

(** A node of the board representing a settlement location in the game *)
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

(** An edge of the board representing a road location in the game *)
type edge = { node1 : node; node2 : node; is_road : bool }

module type BoardType = sig
  type t

  (** Takes in all nodes on the board for a given game (in given state at that time), 
      and constructs the adgacency list of other neighboring nodes in the game, 
      returning the adgacency list. A neighbor is Some node, and a lack of neighbor is none *)
  val build_vertices : node list -> node option list list

  (** All edges in the game board *)
  val edges : edge list

  (** All nodes in the game board *)
  val nodes : node list

  (** A board representing the initial board before any pieces are placed *)
  val initial_board : t
  
  (** Builds a road on the map at edge road_loc, returns the updated board. 
      If the move is illegal, it will return None (a move is illegal if there is already a piece at the desired location) *)
  val build_road : t -> int -> t option

  (** Builds a settlement on the map at vertex settlement_loc, returns the updated board. 
    If the move is illegal, it will return None (a move is illegal if there is already a piece at the desired location) *)
  val build_settlement : t -> int -> t option

  (** Builds a city on the map at vertex city_loc, returns the updated board. 
  If the move is illegal, it will return None (a move is illegal if there is already a piece at the desired location) *)
  val build_city : t -> int -> t option  

  (** Takes in a node and returns the board's corresponding adjacency list *)
  val get_adj_lst : t -> node option list list

  (** Takes in a node and returns the board's corresponding node list *)
  val get_node_lst : t -> node list

  (** Takes in a node and returns the board's corresponding edge list *)
  val get_edge_lst : t -> edge list

  (** Takes in a node and returns the resource of the first neighbor *)
  val get_node_border_one_resource : node -> resource option

  (** Takes in a node and returns the roll number of the first neighbor *)
  val get_node_border_one_number : node -> int option

  (** Takes in a node and returns the resource of the second neighbor *)
  val get_node_border_two_resource : node -> resource option  

  (** Takes in a node and returns the roll number of the second neighbor *)
  val get_node_border_two_number : node -> int option

  (** Takes in a node and returns the resource of the third neighbor *)
  val get_node_border_three_resource : node -> resource option

  (** Takes in a node and returns the roll number of the third neighbor *)
  val get_node_border_three_number : node -> int option
end

module SmallBoard: BoardType