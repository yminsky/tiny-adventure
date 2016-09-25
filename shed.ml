open Import

let here = Room.Shed
let things = []

let desc _ = {|
You're standing in front of a gray shed with a rickety looking 
door. There's a small plaque to the right of the door, and there
are leaves and rocks scattered on the floor in front of the door.
|}

let plaque_desc = {|
Welcome intrepid adventurers! If you're here, then surely
you're interested in a life of awesome exploits and terrifying
dangers to test your mettle. 

If so, then your first test is whether you can find a way past
the door.
|}

let run (state:State.t) : run_response =
  match prompt () with
  | Dir West -> (state, Road 0)
  | Look_at (_,"plaque") ->
    sayf "It's a small bronze plaque, with intricate writing on it.";
    (state, here)
  | Read "plaque" ->
    sayf "%s" plaque_desc;
    (state, here)
  | Open "door" ->
    if Set.mem state.inventory Rusty_key then (
      sayf {|
You put the key in the lock and turn. It grinds to the right and
the door swings open.|};
      (State.assert_fact state Shed_door_is_open, here)
    ) else (
      sayf {|
You try, but it's locked. It's surprisingly sturdy for \n\
a shack that looks pretty beat up.|};
      (state, here)
    )
  | Enter (Some "shed") | Enter None ->
    if State.is_fact state Shed_door_is_open then (
      sayf {|
You stop inside the shed and notice that it seems a good bit bigger
on the inside than it did on the outside.|};
      print_newline ();
      (state,Inside_shed)
    ) else (
      sayf {|
You try to go in, but your nose runs painfully into the door.
Next time you might want to try opening it first.|};
      (state,here)
    )
  | Look_at (At, "rocks") ->
    sayf "They look gray and rocky.";
    (state,here)
  | Look_at (At,"leaves") ->
    if not (State.is_fact state Rusty_key_was_found) then (
      sayf {|
They look brown and crinkly. You think you catch a glint of 
something underneath, though... |}
    ) else (
      sayf "They look brown and crinkly."
    );
    (state,here)
  | Move "rocks" ->
    sayf {|
You move the rocks around aimlessly, eventually finding an arrangement
you find visually pleasing. You're pretty good at this! Have you
considered a career for you in Feng Shui?
|};
    (state,here)
  | Look_at (Under,"leaves") | Move "leaves"
    when not (State.is_fact state Rusty_key_was_found) ->
    sayf {|
You move the leaves aside, and you see a small, rusty key,
which you pick up.|};
    let state = 
      { state with
        inventory = Set.add state.inventory Rusty_key }
    in
    (State.assert_fact state Rusty_key_was_found, here)
  | ans -> 
    otherwise ans ~things:["shed";"door";"plaque";"leaves";"rocks"] state here

