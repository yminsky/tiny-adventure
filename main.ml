open Import

let state =
  state_of_rooms
    [ (module Armory)
    ; (module Corridor_1)
    ; (module Corridor_2)
    ; (module Dragon_lair)
    ; (module Exit_from_lair)
    ; (module Inside_shed)
    ; (module Road)
    ; (module Shed)
    ]

let () = 
  State.run state (Road 2)
