type resource = Clay | Wood | Sheep | Ore | Wheat

module type PlayerType = sig
  type t

  val empty : t

  val add_resource : resource -> t -> t

  val build_road : t -> t

  val build_settlment : t -> t

  val build_city : t -> t

  (* val add_dev_card : t -> t

  val play_dev_card : t -> t *)

end

module Player = struct
  type t = {clay_count : int; wood_count : int; sheep_count : int; ore_count : int; wheat_count : int; 
            settlement_count : int; road_count : int; city_count : int; dev_card_count : int; 
            army_count : int; long_road_count : int; victory_points : int}

  let empty = {clay_count = 0; wood_count = 0; sheep_count = 0; ore_count = 0; wheat_count = 0;
               settlement_count = 5; road_count = 15; city_count = 3; dev_card_count = 0;
               army_count = 0; long_road_count = 0; victory_points = 0;}
              
  let add_resource card player = match card with
                                | Clay -> let count = (player.clay_count + 1) in {player with clay_count = count} 
                                | Wood -> let count = (player.wood_count + 1) in {player with wood_count = count}  
                                | Sheep -> let count = (player.sheep_count + 1) in {player with sheep_count = count} 
                                | Ore -> let count = (player.ore_count + 1) in {player with ore_count = count} 
                                | Wheat -> let count = (player.wheat_count + 1) in {player with wheat_count = count} 

  let build_road player = {player with clay_count = player.clay_count - 1; wood_count = player.wood_count - 1; road_count = player.road_count - 1}

  let build_settlment player = {player with clay_count = player.clay_count - 1; wood_count = player.wood_count - 1; sheep_count = player.sheep_count - 1; wheat_count = player.wheat_count - 1; settlement_count = player.settlement_count - 1}

  let build_city player = {player with ore_count = player.ore_count - 3; wheat_count = player.wheat_count - 2; city_count = player.city_count - 1}
end
