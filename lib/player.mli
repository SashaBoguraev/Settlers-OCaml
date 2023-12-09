module type PlayerType = sig
  type t
  (**Player with no resources *)
  val empty : t
  (** Builds a road on the map at edge road_loc, returns a tuple of the updated player and an integer 0. 
    If the move is illegal, it will return a tuple of 'None' and a number representing the reason for 
    the error. The key is as follows:
    1: Player doesn't have enough roads to play
    2: Player doesn't have enough resources to play*)
  val build_road : t -> int -> t option * int
  (** Builds a settlment on the map at edge road_loc, returns a tuple of the updated player and an integer 0. 
      If the move is illegal, it will return a tuple of 'None' and a number representing the reason for 
      the error. The key is as follows:
      1: Player doesn't have enough settlements to play
      2: Player doesn't have enough resources to play
      3. It is not a buildable loc *)  
  val build_settlment : t -> int -> t option * int
  (** Builds a city on the map at edge road_loc, returns a tuple of the updated player and an integer 0. 
      If the move is illegal, it will return a tuple of 'None' and a number representing the reason for 
      the error. The key is as follows:
      1: Player doesn't have enough cities to play
      2: Player doesn't have enough resources to play*)
  val build_city : t -> int -> t option * int
  (**Takes in a player, and returns that player's clay count *)
  val get_clay : t -> int
  (**Takes in a player, and returns that player's wood count *)
  val get_wood : t -> int
  (**Takes in a player, and returns that player's wheat count *)
  val get_wheat : t -> int
  (**Takes in a player, and returns that player's sheep count *)
  val get_sheep : t -> int
  (**Takes in a player, and returns that player's ore count *)
  val get_ore : t -> int
  (**Takes in a player, and returns that player's settlement count *)
  val get_settlement_count : t -> int
  (**Takes in a player, and returns that player's road count *)
  val get_road_count : t -> int
  (**Takes in a player, and returns that player's city count *)
  val get_city_count : t -> int
  (**Takes in a player, and returns that player's total victory points*)
  val get_victory_points : t -> int
  (**Takes in a player, and returns a list of that player's settlement locations *)
  val get_settlement_locations : t -> int list
  (**Takes in a player, and returns a list of that player's road locations *)
  val get_road_locations : t -> int list
  (**Takes in a player, and returns a list of that player's city locations *)
  val get_city_locations : t -> int list
  (**Takes in a player, and returns a list of that player's possible buildable locations *)
  val get_buildable_locs : t -> int list
  (**Takes in a player, and returns a player with one more ore *)
  val add_ore : t -> t
  (**Takes in a player, and returns a player with one more wood *)
  val add_wood : t -> t
  (**Takes in a player, and returns a player with one more clay *)
  val add_clay : t -> t
  (**Takes in a player, and returns a player with one more sheep *)
  val add_sheep : t -> t
  (**Takes in a player, and returns a player with one more wheat *)
  val add_wheat : t -> t
  (**Takes in a player, and returns a player with one more point *)
  val add_point : t -> t
  (**Takes in a player, and returns a player with updated resource count after trading in ore *)
  val trade_ore : t -> t
  (**Takes in a player, and returns a player with updated resource count after trading in wood *)
  val trade_wood : t -> t
  (**Takes in a player, and returns a player with updated resource count after trading in clay *)
  val trade_clay : t -> t
  (**Takes in a player, and returns a player with updated resource count after trading in sheep *)
  val trade_sheep : t -> t
  (**Takes in a player, and returns a player with updated resource count after trading in wheat *)
  val trade_wheat : t -> t
end

module Player : PlayerType
