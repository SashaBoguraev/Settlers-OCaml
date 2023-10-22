open Player

type resource = Clay | Wood | Sheep | Ore | Wheat

type node = {border_one_resource: resource option; border_one_number: int option; border_two_resource: resource option; border_two_number: int option; border_three_resource: resource option; border_three_number: int option; is_settlement: bool; is_city: bool}
type edge = {node1: node; node2: node; is_road: bool}

module type BoardType = sig
  type t

  val initial: t

  (* val build_road : t -> t

  val build_settlment : t -> t

  val build_city : t -> t *)
end

module SmallBoard: BoardType = struct
  type t = (node option) list list

  module Player_one = Player.empty
  module Player_two = Player.empty
  
  let node_one: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Sheep; border_three_number = Some 6;}
  
  let node_two: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Wood; border_three_number = Some 4;}
  
  let node_three: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Sheep; border_three_number = Some 6;}

  let node_four: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Sheep; border_two_number = Some 6;
                        border_three_resource = Some Wood; border_three_number = Some 4;}
  
  let node_five: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Wood; border_two_number = Some 4;
                        border_three_resource = None; border_three_number = None;}
  
  let node_six: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Sheep; border_two_number = Some 6;
                        border_three_resource = Some Clay; border_three_number = Some 3;}
  
  let node_seven: node = {border_one_resource = Some Sheep; border_one_number = Some 6;
                        border_two_resource = Some Wood; border_two_number = Some 4;
                        border_three_resource = None; border_three_number = None;}
  
  let node_eight: node = {border_one_resource = Some Wood; border_one_number = Some 4;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Wood; border_three_number = Some 2;}
  
  let node_nine: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Clay; border_three_number = Some 3;}

  let node_ten: node = {border_one_resource = Some Sheep; border_one_number = Some 6;
                        border_two_resource = Some Clay; border_two_number = Some 10;
                        border_three_resource = None; border_three_number = None;}
  
  let node_eleven: node = {border_one_resource = Some Wood; border_one_number = Some 4;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Wood; border_three_number = Some 2;}
  
  let node_twelve: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Wood; border_two_number = Some 2;
                        border_three_resource = None; border_three_number = None;}

  let node_thirteen: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Clay; border_two_number = Some 3;
                        border_three_resource = None; border_three_number = None;}
  
  let node_fourteen: node = {border_one_resource = Some Clay; border_one_number = Some 3;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Wheat; border_three_number = Some 5;}
  
  let node_fifteen: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Wood; border_two_number = Some 2;
                        border_three_resource = Some Ore; border_three_number = Some 1;}

  let node_sixteen: node = {border_one_resource = Some Wood; border_one_number = Some 2;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = None; border_three_number = None;}
  
  let node_seventeen: node = {border_one_resource = Some Clay; border_one_number = Some 3;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = Some Sheep; border_three_number = Some 6;}
  
  let node_eighteen: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Wheat; border_two_number = Some 5;
                        border_three_resource = Some Ore; border_three_number = Some 1;}
  
  let node_nineteen: node = {border_one_resource = Some Wood; border_one_number = Some 2;
                        border_two_resource = Some Ore; border_two_number = Some 1;
                        border_three_resource = None; border_three_number = None;}
  
  let node_twenty: node = {border_one_resource = None; border_one_number = None;
                        border_two_resource = Some Wheat; border_two_number = Some 5;
                        border_three_resource = None; border_three_number = None;}
  
  let node_twentyone: node = {border_one_resource = Some Wheat; border_one_number = Some 5;
                        border_two_resource = Some Ore; border_two_number = Some 1;
                        border_three_resource = None; border_three_number = None;}

  let node_twentytwo: node = {border_one_resource = Some Ore; border_one_number = Some 1;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = None; border_three_number = None;}
  
  let node_twentythree: node = {border_one_resource = Some Wheat; border_one_number = Some 5;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = None; border_three_number = None;}
  
  let node_twentyfour: node = {border_one_resource = Some Ore; border_one_number = Some 1;
                        border_two_resource = None; border_two_number = None;
                        border_three_resource = None; border_three_number = None;}

  let initial = [
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

end