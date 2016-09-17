open! Import

let printf = Printf.printf
let st = String.strip

let sayf format =
  Printf.ksprintf print_endline format

let prompt () =
  printf "\n> %!";
  match input_line stdin with
  | exception _ -> Other []
  | x ->
    let words = 
      let open List.Let_syntax in
      String.split x ~on:' '
      >>| String.strip
      >>| String.lowercase
      >>| String.filter ~f:Char.is_alphanum
    in
    Parser.run words

(** ROOMS **)

module State = struct
  type room_f = t -> t * Room.t

  and t = 
    { rooms : room_f Map.M(Room).t
    ; room_things : Set.M(Thing).t Map.M(Room).t
    ; inventory : Set.M(Thing).t
    ; facts : Set.M(Fact).t
    ; descriptions : string Map.M(Room).t
    }

  let empty =
    { rooms = Map.empty (module Room)
    ; room_things = Map.empty (module Room)
    ; inventory = Set.empty (module Thing)
    ; facts = Set.empty (module Fact)
    ; descriptions = Map.empty (module Room)
    }

  let find_things_in_room t room =
    Map.find t.room_things room
    |> Option.value ~default:(Set.empty (module Thing))

  let take t room thing =
    let things_in_room = find_things_in_room t room in
    if not (Set.mem things_in_room thing) then None
    else
      let things_in_room = Set.remove things_in_room thing in
      let inventory = Set.add t.inventory thing in
      let room_things = Map.add t.room_things ~key:room ~data:things_in_room in
      Some { t with inventory; room_things }

  let drop t room thing =
    if not (Set.mem t.inventory thing) then None
    else
      let things_in_room = 
        Set.add (find_things_in_room t room) thing
      in 
      let inventory = Set.remove t.inventory thing in
      let room_things = Map.add t.room_things ~key:room ~data:things_in_room in
      Some { t with inventory; room_things }

end

let state_ref = ref State.empty

(** Lets you register a room. Rooms must be functions that take their
    own location as an argument. This gives a natural way for room
    functions to know their own location, which turns out to be
    broadly useful. *)
let register room room_f room_desc =
  let room_f = room_f room in
  state_ref :=
    { !state_ref with
      rooms = Map.add (!state_ref).rooms ~key:room ~data:room_f
    ; descriptions = Map.add (!state_ref.descriptions) ~key:room ~data:room_desc
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

let print_description (state:State.t) room =
  begin match Map.find state.descriptions room with
  | None -> ()
  | Some desc ->
    print_endline desc
  end;
  match Map.find state.room_things room with
  | None -> ()
  | Some things ->
    Set.iter things ~f:(fun thing ->
      sayf "You see a %s" (Thing.to_string thing)
    )
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
    print_description state room;
    (state,room)
  | Look_at s ->
    begin
      if List.mem things s 
      then sayf "It looks like a perfectly ordinary %s." s
      else printf "I see no %s here." s
    end;
    (state,room)
  | Read s ->
    if String.equal s "book" 
    then sayf "You think back to your copy of Land of Stories, and are sad \n\
               you don't have it with you."
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

let road_desc n =
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
  | Look_at "shed" when n = 0 ->
    sayf "It doesn't look like much from here. Maybe take a closer look?";
    (state,here)
  | ans -> 
    otherwise ans ~things:["road";"dirt"] state here

let () = register (Road 2) (road 2) (road_desc 2)

let shed_desc = st {|
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
  | Look_at "plaque" ->
    sayf "It's a small bronze plaque, with intricate writing on it.";
    (state, here)
  | Read "plaque" ->
    print_endline plaque_desc;
    (state, here)
  | Open "door" ->
    if Set.mem state.inventory Rusty_key then (
      sayf "You put the key in the lock and turn. It grinds to the right and \n\
            the door swings open.";
      let state = { state with facts = Set.add state.facts Shed_door_is_open } in
      (state,here)
    ) else (
      sayf "You try, but it's locked. It's surprisingly sturdy for \n\
            a shack that looks pretty beat up.";
      (state, here)
    )
  | Enter "door" ->
    if Set.mem state.facts Shed_door_is_open 
    then (state,Inside_shed)
    else (
      sayf "You try to go in, but your nose runs painfully into the door.\n\
            Next time you might want to try opening it first.";
      (state,here)
    )
  | Look_at "leaves" ->
    sayf "You look at the leaves and see something glint.  Reaching down, \n\
          you see a small, rusty key, which you pick up.";
    let state = 
      { state with
        inventory = Set.add state.inventory Rusty_key }
    in
    (state, here)
  | ans -> 
    otherwise ans ~things:["shed";"door";"plaque";"leaves";"rocks"] state here

let () = register Shed shed shed_desc


let inside_shed_desc = st {|
You stop inside the shed and notice that it seems a good bit bigger
on the inside than it did on the outside. You had to bend over to 
get inside, but as you stand up and look around you realize you're
in a large room with smooth granite walls.

There are torches on the walls, which cast a wavering orange light.
|}

let inside_shed here (state:State.t) : (_ * Room.t) =
  otherwise (prompt ()) ~things:["wall";"walls";"torch";"torches"]
    state here

let () = register Inside_shed inside_shed inside_shed_desc


(** The runtime for executing the game *)

let rec run (state:State.t) ~old room =
  match Map.find state.rooms room with
  | None -> sayf "Huh. I'm lost. Game over."
  | Some f ->
    if not (Room.equal old room) then print_description state room;
    let (state',room') = f state in
    run state' ~old:room room'

let () =
  run !state_ref ~old:Nowhere (Road 2)
