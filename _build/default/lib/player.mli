<<<<<<< HEAD
module type PlayerType = sig
    type t

    type resource
=======
type resource

module type PlayerType = sig
    type t
>>>>>>> main
  
    val empty : t
  
    val add_resource : resource -> t -> t
  
    val build_road : t -> t
  
    val build_settlment : t -> t
  
    val build_city : t -> t
  
    (* val add_dev_card : t -> t
  
    val play_dev_card : t -> t *)
  
end

module Player : PlayerType