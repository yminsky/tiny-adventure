open Import

(* A cute type-trick to make sure we list all relevant rooms here *)
let rooms =
  List.bind Room.all ~f:(function
    | Special _ -> []
    | Armory         -> [(module Armory : Room_definition)]
    | Corridor_1     -> [(module Corridor_1)]
    | Corridor_2     -> [(module Corridor_2)]
    | Dragon_lair    -> [(module Dragon_lair)]
    | Exit_from_lair -> [(module Exit_from_lair)]
    | Inside_shed    -> [(module Inside_shed)]
    | Road _         -> [(module Road.Start); (module Road.Connection)]
    | Shed           -> [(module Shed)]
  )
;;

let () = 
  State.run (State.of_rooms rooms) Road.Start.here
