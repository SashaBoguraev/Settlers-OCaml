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
  val get_dev_card_count : t -> int
  val get_army_count : t -> int
  val get_long_road : t -> int
  val get_victory_points : t -> int
  val get_settlement_locations : t -> int list
  val get_road_locations : t -> int list
  val get_city_locations : t -> int list
  val add_ore : t -> t
  val add_wood : t -> t
  val add_clay : t -> t
  val add_sheep : t -> t
  val add_wheat : t -> t

  (* val add_dev_card : t -> t

     val play_dev_card : t -> t *)
end

module Player : PlayerType
