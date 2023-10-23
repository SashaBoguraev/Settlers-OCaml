type resource

module type PlayerType = sig
    type t
  
    val empty : t
  
    val add_resource : resource -> t -> t
  
    val build_road : t -> int -> t option * int
  
    val build_settlment : t -> int -> t option * int
  
    val build_city : t -> int -> t option * int
  
    (* val add_dev_card : t -> t
  
    val play_dev_card : t -> t *)
  
  end

module Player : PlayerType


<<<<<<< HEAD
module Player : PlayerType
=======
>>>>>>> main
