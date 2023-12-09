module type PlayerType = sig
  type t

  val empty : t
  val build_road : t -> int -> t option * int
  val build_settlment : t -> int -> t option * int
  val build_city : t -> int -> t option * int
  val get_clay : t -> int
  val get_wood : t -> int
  val get_wheat : t -> int
  val get_sheep : t -> int
  val get_ore : t -> int
  val get_settlement_count : t -> int
  val get_road_count : t -> int
  val get_city_count : t -> int
  val get_victory_points : t -> int
  val get_settlement_locations : t -> int list
  val get_road_locations : t -> int list
  val get_city_locations : t -> int list
  val get_buildable_locs : t -> int list
  val add_ore : t -> t
  val add_wood : t -> t
  val add_clay : t -> t
  val add_sheep : t -> t
  val add_wheat : t -> t
  val add_point : t -> t
  val trade_ore : t -> t
  val trade_wood : t -> t
  val trade_clay : t -> t
  val trade_sheep : t -> t
  val trade_wheat : t -> t
end

module Player = struct
  type t = {
    buildable_locs : int list;
    clay_count : int;
    wood_count : int;
    sheep_count : int;
    ore_count : int;
    wheat_count : int;
    settlement_count : int;
    road_count : int;
    city_count : int;
    victory_points : int;
    settlement_locations : int list;
    road_locations : int list;
    city_locations : int list;
  }

  (** Takes in a player, and returns that player's clay count *)
  let get_clay player = player.clay_count

  (** Takes in a player, and returns that player's wood count *)
  let get_wood player = player.wood_count

  (** Takes in a player, and returns that player's wheat count *)
  let get_wheat player = player.wheat_count

  (** Takes in a player, and returns that player's sheep count *)
  let get_sheep player = player.sheep_count

  (** Takes in a player, and returns that player's ore count *)
  let get_ore player = player.ore_count

  (** Takes in a player, and returns that player's settlement count *)
  let get_settlement_count player = player.settlement_count

  (** Takes in a player, and returns that player's road count *)
  let get_road_count player = player.road_count

  (** Takes in a player, and returns that player's city count *)
  let get_city_count player = player.city_count

  (** Takes in a player, and returns that player's total victory points*)
  let get_victory_points player = player.victory_points

  (** Takes in a player, and returns a list of that player's settlement locations *)
  let get_settlement_locations player = player.settlement_locations

  (** Takes in a player, and returns a list of that player's road locations *)
  let get_road_locations player = player.road_locations

  (** Takes in a player, and returns a list of that player's city locations *)
  let get_city_locations player = player.city_locations

  (** Takes in a player, and returns a list of that player's possible buildable locations *)
  let get_buildable_locs player = player.buildable_locs

  (** Takes in a player, and returns a player with updated resource count after trading in clay *)
  let trade_clay player =
    let count = player.clay_count - 3 in
    { player with clay_count = count }

  (** Takes in a player, and returns a player with updated resource count after trading in ore *)
  let trade_ore player =
    let count = player.ore_count - 3 in
    { player with ore_count = count }

  (** Takes in a player, and returns a player with updated resource count after trading in sheep *)
  let trade_sheep player =
    let count = player.sheep_count - 3 in
    { player with sheep_count = count }

  (** Takes in a player, and returns a player with updated resource count after trading in wood *)
  let trade_wood player =
    let count = player.wood_count - 3 in
    { player with wood_count = count }

  (** Takes in a player, and returns a player with updated resource count after trading in wheat *)
  let trade_wheat player =
    let count = player.wheat_count - 3 in
    { player with wheat_count = count }

  (** Player with no resources *)
  let empty =
    {
      buildable_locs = [];
      clay_count = 4;
      wood_count = 4;
      sheep_count = 2;
      ore_count = 0;
      wheat_count = 2;
      settlement_count = 5;
      road_count = 15;
      city_count = 3;
      victory_points = 0;
      settlement_locations = [];
      road_locations = [];
      city_locations = [];
    }

  (** Takes in a player, and returns a player with one more ore *)
  let add_ore player =
    let count = player.ore_count + 1 in
    { player with ore_count = count }

  (** Takes in a player, and returns a player with one more wood *)
  let add_wood player =
    let count = player.wood_count + 1 in
    { player with wood_count = count }

  (** Takes in a player, and returns a player with one more wheat *)
  let add_wheat player =
    let count = player.wheat_count + 1 in
    { player with wheat_count = count }

  (** Takes in a player, and returns a player with one more clay *)
  let add_clay player =
    let count = player.clay_count + 1 in
    { player with clay_count = count }

  (** Takes in a player, and returns a player with one more sheep *)
  let add_sheep player =
    let count = player.sheep_count + 1 in
    { player with sheep_count = count }

  (** Takes in a player, and returns a player with one more point *)
  let add_point player =
    let count = player.victory_points + 1 in
    { player with victory_points = count }

  let edge_road_pairs =
    [
      (1, 3);
      (1, 4);
      (2, 4);
      (2, 5);
      (3, 6);
      (4, 7);
      (5, 8);
      (6, 9);
      (6, 10);
      (7, 10);
      (7, 11);
      (8, 11);
      (8, 12);
      (9, 13);
      (10, 14);
      (11, 15);
      (12, 16);
      (13, 17);
      (14, 17);
      (14, 18);
      (15, 18);
      (15, 19);
      (19, 16);
      (17, 20);
      (18, 21);
      (19, 22);
      (20, 23);
      (21, 23);
      (21, 24);
      (22, 24);
    ]

  (** Builds a road on the map at edge road_loc, returns a tuple of the updated player and an integer 0. 
      If the move is illegal, it will return a tuple of 'None' and a number representing the reason for 
      the error. The key is as follows:
      1: Player doesn't have enough roads to play
      2: Player doesn't have enough resources to play*)
  let build_road (player : t) (loc : int) : t option * int =
    if
      (not
         (List.mem
            (fst (List.nth edge_road_pairs (loc - 1)))
            player.buildable_locs))
      && not
           (List.mem
              (snd (List.nth edge_road_pairs (loc - 1)))
              player.buildable_locs)
    then (None, 3)
    else if
      player.clay_count > 0 && player.wood_count > 0 && player.road_count > 0
    then
      ( Some
          {
            player with
            clay_count = player.clay_count - 1;
            wood_count = player.wood_count - 1;
            road_count = player.road_count - 1;
            road_locations = (loc - 1) :: player.road_locations;
            buildable_locs =
              fst (List.nth edge_road_pairs (loc - 1))
              :: snd (List.nth edge_road_pairs (loc - 1))
              :: player.buildable_locs;
          },
        0 )
    else if player.road_count == 0 then (None, 1)
    else (None, 2)

  (** Builds a settlment on the map at edge road_loc, returns a tuple of the updated player and an integer 0. 
      If the move is illegal, it will return a tuple of 'None' and a number representing the reason for 
      the error. The key is as follows:
      1: Player doesn't have enough settlements to play
      2: Player doesn't have enough resources to play
      3. It is not a buildable loc *)
  let build_settlment player loc =
    if
      (not (List.mem loc player.buildable_locs))
      && get_settlement_count player < 4
    then (None, 3)
    else if
      player.clay_count > 0 && player.wood_count > 0 && player.sheep_count > 0
      && player.wheat_count > 0
      && player.settlement_count > 0
    then
      ( Some
          {
            player with
            clay_count = player.clay_count - 1;
            wood_count = player.wood_count - 1;
            sheep_count = player.sheep_count - 1;
            wheat_count = player.wheat_count - 1;
            settlement_count = player.settlement_count - 1;
            settlement_locations = (loc - 1) :: player.settlement_locations;
            buildable_locs = loc :: player.buildable_locs;
          },
        0 )
    else if player.settlement_count == 0 then (None, 1)
    else (None, 2)

  (** Builds a city on the map at edge road_loc, returns a tuple of the updated player and an integer 0. 
      If the move is illegal, it will return a tuple of 'None' and a number representing the reason for 
      the error. The key is as follows:
      1: Player doesn't have enough cities to play
      2: Player doesn't have enough resources to play*)
  let build_city player loc =
    if player.ore_count > 2 && player.wheat_count > 1 && player.city_count > 0
    then
      ( Some
          {
            player with
            ore_count = player.ore_count - 3;
            wheat_count = player.wheat_count - 2;
            city_count = player.city_count - 1;
            settlement_locations =
              List.filter
                (fun x -> not (x = loc - 1))
                player.settlement_locations;
            city_locations = (loc - 1) :: player.city_locations;
            buildable_locs = loc :: player.buildable_locs;
          },
        0 )
    else if player.city_count == 0 then (None, 1)
    else (None, 2)
end