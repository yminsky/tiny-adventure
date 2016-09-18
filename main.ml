open Import

(* A cute type-trick to make sure we list all relevant rooms here *)
let rooms =
  let room (m : (module Room_definition)) = Some m in
  List.filter_map Room.all ~f:(function
    | Special _ -> None
    | Armory         -> room (module Armory)
    | Corridor_1     -> room (module Corridor_1)
    | Corridor_2     -> room (module Corridor_2)
    | Dragon_lair    -> room (module Dragon_lair)
    | Exit_from_lair -> room (module Exit_from_lair)
    | Inside_shed    -> room (module Inside_shed)
    | Road _         -> room (module Road)
    | Shed           -> room (module Shed)
  )
;;

let () = 
  State.run (State.of_rooms rooms) (Road 2)
