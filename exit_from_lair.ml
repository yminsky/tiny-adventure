open Import

let here = Room.Exit_from_lair
let things = []
let desc _ = {|
The corridor out of the lair angles up, and soon enough you can feel a
change to the taste of the air. Moments later you see light, and then,
finally, you're above ground.

That was an oddly disturbing experience, but you can't help hoping for
a future time when your father has had the opportunity to write some
more rooms.

Congratulations. You've survived to live another day.

If you want to keep that up, I recommend going on fewer adventures.|}

let run state : run_response = (state,Special Exit)
