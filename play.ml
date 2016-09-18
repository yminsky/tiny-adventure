open! Import
open Util

type resp = State.t * Room.t

let state_ref = 
  ref State.empty

(** Lets you register a room. Rooms must be functions that take their
    own location as an argument. This gives a natural way for room
    functions to know their own location, which turns out to be
    broadly useful. *)
let register ?things room room_desc room_f =
  let state = !state_ref in
  let room_f = room_f room in
  let room_things =
    match things with
    | None -> state.room_things
    | Some things ->
      Map.add state.room_things ~key:room
        ~data:(Set.of_list (module Thing) things)
  in
  state_ref :=
    { state with
      rooms = Map.add state.rooms ~key:room ~data:room_f
    ; descriptions = Map.add state.descriptions ~key:room ~data:room_desc
    ; room_things
    }

let drop state room thing_s =
  let action = 
    let open Option.Let_syntax in
    let%bind thing = Thing.of_string thing_s in
    State.drop state room thing
  in
  match action with
  | Some state' -> (state',room)
  | None ->
    sayf "Well, you can't drop what you don't have.";
    (state,room)
;;

let take state room thing_s = 
  let action =
    let open Option.Let_syntax in
    let%bind thing = Thing.of_string thing_s in
    State.take state room thing
  in
  match action with
  | Some state' -> (state',room)
  | None ->
    sayf "I can't take that.";
    (state,room)
;;

let inventory (state:State.t) room =
  if Set.is_empty state.inventory then (
    sayf "Man, you got nothing.";
    (state,room)
  ) else (
    Set.iter state.inventory ~f:(fun thing ->
      sayf "You have a %s" (Thing.to_string thing));
    (state,room)
  )
;;

let first_help = "Oh, don't be such a baby. You can figure this out."
let second_help_prelude = {|
Ok, fine. I'll give you some hints. I can't stand to see you
so hopeless.|}
let third_help = {|

If you want to know what you're carrying, you can type
"inventory" (or "i", for short.)

Also, if you want to see the description of a room again, type
"look".

Other than that, just try things out! Trying to figure out what
sentences I'll understand is part of the frustration, uh, I mean
fun!
|}
let second_help = second_help_prelude ^ third_help


(** Default handling. This is meant to automate things that you'd
    otherwise have to implement in each room separately. *)
let otherwise ~things (ans:Answer.t) (state:State.t) (here:Room.t) =
  begin match ans with
  | Dir _ ->
    sayf "You can't go that way.";
    (state,here)
  | Take s -> take state here s
  | Drop s -> drop state here s
  | Look ->
    State.print_description state here;
    (state,here)
  | Look_at (_,s) ->
    begin
      if List.mem things s 
      then sayf "It looks like a perfectly ordinary %s." s
      else printf "I see no %s here." s
    end;
    (state,here)
  | Read s ->
    if String.equal s "book" 
    then sayf {|
You think back to your copy of Land of Stories, and are sad
 you don't have it with you.|}
    else if List.mem things s 
    then sayf "That is hardly a gripping read."
    else sayf "I don't see a %s worth reading." s;
    (state,here)
  | Open s ->
    (if List.mem things s 
     then sayf "I don't know how to open that."
     else sayf "I don't see a %s to open." s);
    (state,here)
  | Enter s ->
    if List.mem things s
    then (sayf "I can't enter that!")
    else (sayf "I don't see a %s to enter" s);
    (state,here)
  | Help ->
    if not (State.is_fact state Asked_for_help) then (
      sayf "%s" first_help;
      (State.assert_fact state Asked_for_help, here)
    ) else if not (State.is_fact state Asked_for_help_again) then (
      sayf "%s" second_help;
      (State.assert_fact state Asked_for_help_again, here)
    ) else (
      sayf "%s" third_help;
      (state,here)
    )
  | Inventory -> inventory state here
  | Save -> (state,Save)
  | Load -> (state,Load)
  | Other _ ->
    sayf "Sorry, I didn't understand that.";
    (state,here)
  end
;;


let generic_road_desc = {|
You are standing on the side of a deserted dirt road.  The sky is 
gray, and there's a cold wind blowing. The road stretches to the 
north and south.
|}

let house_desc = {|
You see a small wooden shed off to the east.
|}

let road_desc n _state =
  if n = 0 then 
    String.strip generic_road_desc
    ^ "\n\n"
    ^ String.strip house_desc
  else generic_road_desc

let rec road n here state : State.t * Room.t =
  let add_room m (state:State.t) =
    let key = Room.Road m in
    let rooms = 
      Map.add state.rooms ~key
        ~data:(road m (Room.Road m)) 
    in
    let descriptions =
      Map.add state.descriptions ~key
        ~data:(road_desc m)
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

let () = register (Road 2) (road_desc 2) (road 2)

let shed_desc _ = {|
You're standing in front of a gray shed with a rickety looking 
door. There's a small plaque to the right of the door, and there
are leaves and rocks scattered on the floor in front of the door.
|}

let plaque_desc = {|
Welcome intrepid adventurers! If you're here, then surely
you're interested in a life of awesome exploits and terrifying
danger that test your mettle. 

If so, then your first test is whether you can find a way past
the door.

(Note from the proprieters: it's not yet possible to get through
the door, but there will be soon.)
|}

let shed here (state:State.t) : (_ * Room.t) =
  match prompt () with
  | Dir West -> (state, Road 0)
  | Look_at (_,"plaque") ->
    sayf "It's a small bronze plaque, with intricate writing on it.";
    (state, here)
  | Read "plaque" ->
    print_endline plaque_desc;
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
  | Enter "door" ->
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
  | Look_at (At,"leaves") ->
    sayf {|
They look brown and crinkly. You think you catch a glint of 
something underneath, though... |};
    (state,here)
  | Look_at (Under,"leaves") ->
    sayf {|
You move the leaves aside, and you see a small, rusty key,
which you pick up.|};
    let state = 
      { state with
        inventory = Set.add state.inventory Rusty_key }
    in
    (state, here)
  | ans -> 
    otherwise ans ~things:["shed";"door";"plaque";"leaves";"rocks"] state here

let () = register Shed shed_desc shed

let inside_shed_desc _ = {|
You're in a large room with smooth granite walls. There are torches 
on the walls, which cast a wavering orange light.

There are doors to the north and east, and a small wooden sign 
attached to the wall, though you're not sure what's keeping it
there.
|}

let inside_shed here (state:State.t) : (_ * Room.t) =
  match prompt () with
  | Dir North ->
    (state,Corridor_1)
  | Dir East ->
    (state,Corridor_2)
  | Read "sign" ->
    sayf {|
The sign reads: 

    Welcome! Really, you should probably have stayed outside, 
    but now you're stuck. You should probably try to find 
    your way out, and avoid getting eaten, while you're at it.

    Just a warning: darkness is dangerous. You might want to
    take a torch.
|};
    (state,here)
  | Take "torch" ->
    sayf "You're now holding a torch. Careful not to burn anything!";
    let state = { state with inventory = Set.add state.inventory Torch } in
    (state,here)
  | ans ->
    otherwise ans ~things:["wall";"walls";"torch";"torches";"sign"] state here

let () = register Inside_shed inside_shed_desc inside_shed ;;

register Corridor_1
  (fun (state:State.t) ->
     if not (Set.mem state.inventory Torch) then
       "It's pitch black. There's not much to see."
     else {|
The light from your torch flickers and highlights the deep scratches
in the wall. Were they made by claws? As if to answer the question,
you hear the sounds of scraping claws coming from the corridor ahead
of you.

The corridor continues to the north, and back to the south from where
you came.
|})
  (fun here (state:State.t) ->
     match prompt () with
     | Dir _ when not (Set.mem state.inventory Torch) ->
       sayf {|
Wandering around in the dark really is dangerous. As you try to
make your way, you stumble over a rock and fall flat on your face. 
Clearly something was lurking in the dark because the next moment, 
you feel claws grab into your back. |};
       (state,Game_over)
     | Dir North ->
       sayf "You continue down the corridor, until a large cavern opens up";
       (state,Dragon_lair)
     | ans ->
       otherwise ans ~things:[] state here)
;;

register Corridor_2
  (fun _ -> {|
This east-west corridor is smooth-walled, with lit torches in,
the walls.  The floors look like hundreds of thousands of
footsteps have been beaten into it over the years.
|})
  (fun here (state:State.t) ->
     match prompt () with
     | Dir West -> (state, Inside_shed)
     | Dir East -> (state, Armory)
     | ans ->
       otherwise ans ~things:[] state here)
;;

register Armory
  ~things:[Sword]
  (fun _ -> {|
This is clearly an armory, which is to say a place where weapons
were once stored. You can tell because of the racks on the walls
that clearly once held pikes and swords and the like.

But whatever it once was, it's now in disarray. There are piles
of junk everywhere, mostly bits of wood and cloth that probably
had some practical purpose once upon a time.
|})
  (fun here (state:State.t) ->
     match prompt () with
     | Dir West -> (state,Corridor_2)
     | Look_at (Under,("junk"|"wood")) -> 
       if State.is_fact state Armory_junk_examined then (
         sayf {|
Yawn. Your further examination of the junk bores you to tears.|};
         (state,here)
       ) else (
         sayf {|
You look under the piles of wood, and notice a stout-looking,
round wooden shield. You pick it up for a moment, but, surprised
by how light it is, the shield tumbles out of your hands. |};
         let state =
           { state with
             facts = Set.add state.facts Armory_junk_examined
           ; room_things =
               Map.update state.room_things here ~f:(fun set_opt ->
                 let set = 
                   Option.value ~default:(Set.empty (module Thing))
                     set_opt
                 in
                 Set.add set Shield)
           }
         in
         (state,here)
       )
     | ans ->
       otherwise ~things:["racks";"wood";"cloth";"junk"]
         ans state here
  )
;;

let dead_dragon = {|
You see the dragon's body lying on the floor in front of you, 
your sword sticking out if its eye, and blood dripping out.
Frankly, you're shocked you're still alive.

There is an exit to the north and south.|}

let dragon_prelude = {|
There is a dragon, as silent as stone, and its head as large 
as your entire body. The cavern itself is enormous and oddly 
beautiful, with crystals in the wall that glint back at you
like stars in the sky.|}

let dragon_unarmed = {|
But you only have a moment to enjoy it. The dragon's head darts 
down, grabbing your entire body with its long, razor-sharp teeth.
You feel a moment of embarrassment that you weren't scared off by
the claw marks, mixed with intense pain.|}

let dragon_with_sword = {|
The dragon's head looks like it's about to descend upon you when 
it appears to notice the sword in your hand. Not interested in
getting its eye poked out, it instead opens its terrifying jaws
and gouts of fire emerge, engulfing you, and burning your body
to a crisp. 

Mercifully, it didn't last long.|}

let dragon_with_shield = {|
With a stunningly fast side-swipe motion, the dragon knocks the 
shield out of your hand, nearly tacking your hand with it. Then,
it's enormous jaws open wide and close over your head.

The dragon's tonsils are the last thing you ever see.
|}

let dragon_with_both = {|
The dragon looks like it's about to grab you in its teeth, when it
notices the glint of your sword. Thinking better of it, the dragon's
jaws open wide and gouts of fire pour out. You hide pathetically
behind your shield, and, oddly, it works. Indeed, you feel an
almost mechanical thrum as the shield glows and repels the fire,
keeping you feeling oddly cool.

The dragon closes its mouth and looks at you, annoyed. With what
you can only interpret as a shrug of resignation, it seems to come
to a decision. It's head darts down and it makes a side-swiping 
motion as if to knock the shield out of your hand. Instinctivly,
you slash towards the onrushing head with your sword.

Luckily, our sword is fully extended at just the time that the
dragon's head is about to hit you. The dragon's eye smashes 
directly into the tip of your sword, driving the tip deep into
its skull, and nearly breaking your arm.

You were knocked back on the floor, and the wind was knocked out
of you. But after you catch your breath, you realize that no
permanent damage was done.

To you, anyway. The dragon seems to be permanently dead. |}
;;

register Dragon_lair
  (fun state ->
     if State.is_fact state Dragon_is_dead
     then dead_dragon
     else dragon_prelude)
  (fun here (state:State.t) ->
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
  )
;;

register Exit_from_lair
  (fun _ -> {|
The corridor out of the lair angles up, and soon enough
you can feel a change to the taste of the air. Moments
later you see light, and then, finally, you're above 
ground.

That was an oddly disturbing experience, but you can't
help hoping for a future time when your father has had
the opportunity to write some more rooms.

Congratulations. You've survived to live another day.

If you want to keep on doing that, I recommend you 
stop going on adventures.|})
  (fun _ (state:State.t) -> (state,Exit))


let () = State.run !state_ref (Road 2)
