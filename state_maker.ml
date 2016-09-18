open Base

module type Room_definition = sig
  val here : Room.t
  val desc : State.t -> string
  val run : State.t -> State.t * Room.t
  val things : Thing.t list
end

let add_room (state:State.t) (module R : Room_definition) =
  let room_things = 
    match R.things with
    | [] -> state.room_things
    | things ->
      Map.add state.room_things ~key:R.here
        ~data:(Set.of_list (module Thing) things)
  in
  let rooms = Map.add state.rooms ~key:R.here ~data:R.run in
  let descriptions = Map.add state.descriptions ~key:R.here ~data:R.desc in
  { state with rooms; descriptions; room_things }

let state_of_rooms rooms =
  List.fold rooms ~init:State.empty ~f:add_room
