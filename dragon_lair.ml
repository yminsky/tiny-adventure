open Import

let room = Room.Dragon_lair
let things = []

let dead_dragon = {|
You see the dragon's body lying on the floor in front of you, your
sword sticking out if its eye, and blood dripping out. Frankly, you're
shocked you're still alive.

There is an exit to the north and south.|}

let dragon_prelude = {|
There is a dragon, as silent as stone, and its head as large as your
entire body. The cavern itself is enormous and oddly beautiful, with
crystals in the wall that glint back at you like stars in the sky.|}

let dragon_unarmed = {|
But you only have a moment to enjoy it. The dragon's head darts down,
grabbing your entire body with its long, razor-sharp teeth. You feel
a moment of embarrassment that you weren't scared off by the claw
marks, mixed with intense pain.|}

let dragon_with_sword = {|
The dragon's head looks like it's about to descend upon you when it
appears to notice the sword in your hand. Not interested in getting
its eye poked out, it instead opens its terrifying jawsand gouts of
fire emerge, engulfing you, and burning your body to a crisp. 

Mercifully, it didn't last long.|}

let dragon_with_shield = {|
With a stunningly fast side-swipe motion, the dragon knocks the shield
out of your hand, nearly tacking your hand with it. Then, its enormous
jaws open wide and close over your head.

The dragon's tonsils are the last thing you ever see.
|}

let dragon_with_both = {|
The dragon looks like it's about to grab you in its teeth, when it
notices the glint of your sword. Thinking better of it, the dragon's
jaws open wide and gouts of fire pour out. You hide pathetically
behind your shield, and, oddly, it works. Indeed, you feel a
mechanical thrum as the shield glows and repels the fire, keeping you
feeling oddly cool.

The dragon closes its mouth and looks at you, annoyed. With what you
can only interpret as a shrug of resignation, it seems to come to a
decision. Its head darts down and it makes a side-swiping motion as
if to knock the shield out of your hand. Instinctivly, you slash
towards the onrushing head with your sword.

Luckily, our sword is fully extended at just the time that the 
dragon's head is about to hit you. The dragon's eye smashes directly
into the tip of your sword, driving the tip deep into its skull, and
nearly breaking your arm.

You were knocked back on the floor, and the wind was knocked out of
you. But after you catch your breath, you realize that no permanent 
damage was done.

To you, anyway. The dragon seems to be permanently dead. |}
;;

let desc state =
  if State.is_fact state Dragon_is_dead
  then dead_dragon
  else dragon_prelude

let run here (state:State.t) : run_response =
  if State.is_fact state Dragon_is_dead then (
    match prompt () with
    | Take "sword" ->
      sayf {|
The sword is jammed so deep in the dragon's eye socket
that you can't yank it out.|};
      (state,here)
    | Dir North ->
      (state,Exit_from_lair)
    | Dir South ->
      (state, Corridor_1)
    | ans ->
      otherwise ~things:["sword";"dragon"]
        ans state here
  ) else (
    let has_sword = Set.mem state.inventory Sword in
    let has_shield = Set.mem state.inventory Shield in
    print_newline ();
    match has_sword, has_shield with
    | false, false ->
      sayf "%s" dragon_unarmed;
      (state,Game_over)
    | true, false ->
      sayf "%s" dragon_with_sword;
      (state, Game_over)
    | false, true ->
      sayf "%s" dragon_with_shield;
      (state, Game_over)
    | true, true ->
      sayf "%s" dragon_with_both;
      let state =
        { state with
          facts = Set.add state.facts Dragon_is_dead
        ; inventory = Set.remove state.inventory Sword
        }
      in
      (state,here)
  )
;;

