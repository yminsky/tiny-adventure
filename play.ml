open! Import
open Util

type resp = State.t * Room.t

let state_ref = 
  ref State.empty

(** Lets you register a room. Rooms must be functions that take their
    own location as an argument. This gives a natural way for room
    functions to know their own location, which turns out to be
    broadly useful. *)
let register ?things room room_f room_desc =
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

(** Default handling. This is meant to automate things that you'd
    otherwise have to implement in each room separately. *)
let otherwise ~things (ans:Answer.t) (state:State.t) (room:Room.t) =
  begin match ans with
  | Dir _ ->
    sayf "You can't go that way.";
    (state,room)
  | Take s -> take state room s
  | Drop s -> drop state room s
  | Look ->
    State.print_description state room;
    (state,room)
  | Look_at (_,s) ->
    begin
      if List.mem things s 
      then sayf "It looks like a perfectly ordinary %s." s
      else printf "I see no %s here." s
    end;
    (state,room)
  | Read s ->
    if String.equal s "book" 
    then sayf {|
You think back to your copy of Land of Stories, and are sad
 you don't have it with you.|}
    else if List.mem things s 
    then sayf "That is hardly a gripping read."
    else sayf "I don't see a %s worth reading." s;
    (state,room)
  | Open s ->
    (if List.mem things s 
     then sayf "I don't know how to open that."
     else sayf "I don't see a %s to open." s);
    (state,room)
  | Enter s ->
    if List.mem things s
    then (sayf "I can't enter that!")
    else (sayf "I don't see a %s to enter" s);
    (state,room)
  | Other ["help"] ->
    sayf "Oh, don't be such a baby. You can figure this out.";
    (state,room)
  | Inventory -> inventory state room
  | Other _ ->
    sayf "Sorry, I didn't understand that.";
    (state,room)
  end
;;


let generic_road_desc = st {|
You are standing on the side of a deserted dirt road.  The sky is 
gray, and there's a cold wind blowing. The road stretches to the 
north and south.
|}

let house_desc = st {|
You see a small wooden shed off to the east.
|}

let road_desc n _state =
  if n = 0 then generic_road_desc ^ "\n\n" ^ house_desc
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

let () = register (Road 2) (road 2) (road_desc 2)

let shed_desc _ = st {|
You're standing in front of a gray shed with a rickety looking 
door. There's a small plaque to the right of the door, and there
are leaves and rocks scattered on the floor in front of the door.
|}

let plaque_desc = st {|
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
      let state = { state with facts = Set.add state.facts Shed_door_is_open } in
      (state,here)
    ) else (
      sayf {|
You try, but it's locked. It's surprisingly sturdy for \n\
a shack that looks pretty beat up.|};
      (state, here)
    )
  | Enter "door" ->
    if Set.mem state.facts Shed_door_is_open 
    then (
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

let () = register Shed shed shed_desc

let inside_shed_desc = st {|
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

let () = register Inside_shed inside_shed (Fn.const inside_shed_desc)

let corridor_1_desc (state:State.t) =
  if not (Set.mem state.inventory Torch) then
    "It's pitch black. There's not much to see."
  else st {|
The light from your torch flickers and highlights the deep scratches
in the wall. Were they made by claws? As if to answer the question,
you hear the sounds of scraping claws coming from the corridor ahead
of you.

The corridor continues to the north, and back to the south from where
you came.
|}
;;

let corridor_1 here (state:State.t) : resp =
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
    (state,Dragon_room)
  | ans ->
    otherwise ans ~things:[] state here
;;

let () = register Corridor_1 corridor_1 corridor_1_desc

let () = State.run !state_ref (Road 2)
