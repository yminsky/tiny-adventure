open! Base

type room_f = t -> t * Room.t

and t = 
  { rooms : room_f Map.M(Room).t
  ; room_things : Set.M(Thing).t Map.M(Room).t
  ; inventory : Set.M(Thing).t
  ; facts : Set.M(Fact).t
  ; descriptions : string Map.M(Room).t
  }

val empty : t

val find_things_in_room : t -> Room.t -> Set.M(Thing).t
val take : t -> Room.t -> Thing.t -> t option
val drop : t -> Room.t -> Thing.t -> t option
