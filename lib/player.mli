type resource

module type PlayerType = sig
  type t

  val empty : t
  val add_resource : resource -> t -> t
  val build_road : t -> int -> t option * int
  val build_settlment : t -> int -> t option * int
  val build_city : t -> int -> t option * int
  val get_clay : t -> int
  val get_wood : t -> int
  val get_wheat : t -> int
  val get_sheep : t -> int
  val get_ore : t -> int

  (* val add_dev_card : t -> t

     val play_dev_card : t -> t *)
end

module Player : PlayerType
