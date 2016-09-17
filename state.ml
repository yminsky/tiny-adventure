open Base

type room_f = t -> t * Room.t

and t = 
  { rooms : room_f Map.M(Room).t
  ; room_things : Set.M(Thing).t Map.M(Room).t
  ; inventory : Set.M(Thing).t
  ; facts : Set.M(Fact).t
  ; descriptions : string Map.M(Room).t
  }

let empty =
  { rooms = Map.empty (module Room)
  ; room_things = Map.empty (module Room)
  ; inventory = Set.empty (module Thing)
  ; facts = Set.empty (module Fact)
  ; descriptions = Map.empty (module Room)
  }

let find_things_in_room t room =
  Map.find t.room_things room
  |> Option.value ~default:(Set.empty (module Thing))

let take t room thing =
  let things_in_room = find_things_in_room t room in
  if not (Set.mem things_in_room thing) then None
  else
    let things_in_room = Set.remove things_in_room thing in
    let inventory = Set.add t.inventory thing in
    let room_things = Map.add t.room_things ~key:room ~data:things_in_room in
    Some { t with inventory; room_things }

let drop t room thing =
  if not (Set.mem t.inventory thing) then None
  else
    let things_in_room = 
      Set.add (find_things_in_room t room) thing
    in 
    let inventory = Set.remove t.inventory thing in
    let room_things = Map.add t.room_things ~key:room ~data:things_in_room in
    Some { t with inventory; room_things }

