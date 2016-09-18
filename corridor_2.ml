open Import

let room = Room.Corridor_2
let things = []

let desc _ = {|
This east-west corridor is smooth-walled, with lit torches in,
the walls.  The floors look like hundreds of thousands of
footsteps have been beaten into it over the years.
|}

let run here (state:State.t) : run_response =
  match prompt () with
  | Dir West -> (state, Inside_shed)
  | Dir East -> (state, Armory)
  | ans ->
    otherwise ans ~things:[] state here

