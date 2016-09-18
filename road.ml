open Import

(** This module actually manages an infinite set of rooms, but there's
    just one registered to begin with. *)

let generic_road_desc = {|
You are standing on the side of a deserted dirt road.  The sky is 
gray, and there's a cold wind blowing. The road stretches to the 
north and south.
|}

let house_desc = {|
You see a small wooden shed off to the east.
|}

let generic_desc n (_:State.t) =
  if n = 0 then 
    String.strip generic_road_desc
    ^ "\n\n"
    ^ String.strip house_desc
  else generic_road_desc

let rec generic_run n here state : run_response =
  let add_room m (state:State.t) =
    let key = Room.Road m in
    let rooms = 
      Map.add state.rooms ~key
        ~data:(generic_run m (Room.Road m)) 
    in
    let descriptions =
      Map.add state.descriptions ~key
        ~data:(generic_desc m)
    in
    { state with rooms; descriptions }
  in
  match prompt () with
  | Dir North -> (add_room (n + 1) state, Road (n + 1))
  | Dir South -> (add_room (n - 1) state, Road (n - 1))
  | Dir East when n = 0 -> (state,Shed)
  | Look_at (_,"shed") when n = 0 ->
    sayf "It doesn't look like much from here. Maybe take a closer look?";
    (state,here)
  | ans -> 
    otherwise ans ~things:["road";"dirt"] state here

let here = Room.Road 2
let things = []
let desc = generic_desc 2
let run = generic_run 2 here

