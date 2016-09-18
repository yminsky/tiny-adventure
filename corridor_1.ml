open Import

let room = Room.Corridor_1
let things = []

let desc (state:State.t) =
  if not (Set.mem state.inventory Torch) then
    "It's pitch black. There's not much to see."
  else {|
The light from your torch flickers and highlights the deep scratches
in the wall. Were they made by claws? As if to answer the question,
you hear the sounds of scraping claws coming from the corridor ahead
of you.

The corridor continues to the north, and back to the south from where
you came.
|}

let run here (state:State.t) : run_response =
  match prompt () with
  | Dir _ when not (Set.mem state.inventory Torch) ->
    sayf {|
Wandering around in the dark really is dangerous. As you try to make
your way, you stumble over a rock and fall flat on your face. Clearly
something was lurking in the dark because the next moment, you feel
claws grab into your back. |};
    (state,Special Game_over)
  | Dir North ->
    sayf "You continue down the corridor, until a large cavern opens up";
    (state,Dragon_lair)
  | Dir South ->
    (state,Inside_shed)
  | ans ->
    otherwise ans ~things:[] state here
