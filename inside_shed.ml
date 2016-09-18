open Import

let room = Room.Inside_shed
let things = []

let desc _ = {|
You're in a large room with smooth granite walls. There are torches 
on the walls, which cast a wavering orange light.

There are doors to the north and east, and a small wooden sign 
attached to the wall, though you're not sure what's keeping it
there.
|}

let run here (state:State.t) : run_response =
  match prompt () with
  | Dir North ->
    (state,Corridor_1)
  | Dir East ->
    (state,Corridor_2)
  | Read "sign" ->
    sayf {|
The sign reads: 

    Welcome! Really, you should have stayed outside, but now you're
    stuck. You should probably try to find your way out, and avoid
    getting eaten, while you're at it.

    Just a warning: darkness is dangerous. You might want to
    take a torch.

    Signed: The Management
|};
    (state,here)
  | Take "torch" ->
    sayf "You're now holding a torch. Careful not to burn anything!";
    let state = { state with inventory = Set.add state.inventory Torch } in
    (state,here)
  | ans ->
    otherwise ans ~things:["wall";"walls";"torch";"torches";"sign"] state here
