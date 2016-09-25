open Import

(* We walk over [Room.all] to make sure all rooms are set up *)
let rooms =
  List.concat_map Room.all ~f:(function
    | Special _ -> []
    | Armory         -> [(module Armory : Room_definition)]
    | Corridor_1     -> [(module Corridor_1)]
    | Corridor_2     -> [(module Corridor_2)]
    | Dragon_lair    -> [(module Dragon_lair)]
    | Exit_from_lair -> [(module Exit_from_lair)]
    | Inside_shed    -> [(module Inside_shed)]
    | Road n         -> [Road.make n]
    | Shed           -> [(module Shed)]
  )
;;

let () = 
  State.run (State.of_rooms rooms) (Road (-2))
