open! Base

let printf = Printf.printf
let st = String.strip

let sayf format =
  Printf.ksprintf print_endline format

module Answer = struct
  type direction = North | South | East | West
  type t =
    | Dir of direction
    | Take of string
    | Drop of string
    | Look of string
    | Read of string
    | Open of string
    | Enter of string
    | Inventory
    | Other of string list
end 

let parse words : Answer.t =
  let c = String.concat ~sep:" " in
  match words with
  | ["go";"north"] | ["n"] | ["north"] -> Dir North
  | ["go";"south"] | ["s"] | ["south"] -> Dir South
  | ["go";"west"]  | ["w"] | ["west"]  -> Dir West
  | ["go";"east"]  | ["e"] | ["east"]  -> Dir East
  | "take" :: "the" :: x | "take" :: x -> Take (c x)
  | "drop" :: "the" :: x | "drop" :: x -> Drop (c x)
  |  "read" :: "the" :: x | "read" :: x -> Read (c x)
  | "look" :: "at" :: "the" :: x | "look" :: "at" :: x -> Look (c x)
  | "open" :: "the" :: x | "open" :: x -> Open (c x)
  | "enter" :: "the" :: x | "enter" :: x -> Enter (c x)
  | ["inventory"] | ["i"] -> Inventory
  | s -> Other s

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
    parse words

(** ROOMS **)

module Room = struct
  module T = struct
    type t =
      | Road of int
      | Shed
      | Inside_shed
      | Nowhere
    [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make(T)
end


module Thing = struct
  module T = struct
    type t = 
      | Rusty_key
    [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make(T)

  let of_string s =
    match s with
    | "rusty key" -> Some Rusty_key
    | _ -> None

  let to_string = function
    | Rusty_key -> "rusty key"
end

module Fact = struct
  module T = struct
    type t =
      | Shed_door_is_open
    [@@deriving compare, sexp]       
  end
  include T
  include Comparable.Make(T)
end

module State = struct
  type room_f = 
    t -> first_time:bool -> t * Room.t

  and t = 
    { rooms : room_f Map.M(Room).t
    ; room_things : Set.M(Thing).t Map.M(Room).t
    ; inventory : Set.M(Thing).t
    ; facts : Set.M(Fact).t
    }

  let empty =
    { rooms = Map.empty (module Room)
    ; room_things = Map.empty (module Room)
    ; inventory = Set.empty (module Thing)
    ; facts = Set.empty (module Fact)
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
let register room room_f =
  let room_f = room_f room in
  state_ref :=
    { !state_ref with
      rooms = Map.add (!state_ref).rooms ~key:room ~data:room_f
    }

let drop state room thing_s =
  match Thing.of_string thing_s with
  | Some thing -> State.drop state room thing
  | None -> None

(** Default handling. This is meant to automate things that you'd
    otherwise have to implement in each room separately. *)
let otherwise ~things (ans:Answer.t) (state:State.t) (room:Room.t) =
  begin match ans with
  | Dir _ ->
    sayf "You can't go that way.";
    (state,room)
  | Take s ->
    sayf "I don't see a %s to take." s;
    (state,room)
  | Drop s ->
    begin match drop state room s with
    | None ->
      sayf "Well, you can't drop what I don't have";
      (state,room)
    | Some state' ->
      (state',room)
    end
  | Look s ->
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
  | Inventory ->
    Set.iter state.inventory ~f:(fun thing ->
      sayf "You have a %s" (Thing.to_string thing));
    (state,room)
  | Other _ ->
    sayf "Sorry, I didn't understand that.";
    (state,room)
  end
;;


let road_desc = st {|
You are standing on the side of a deserted dirt road.  The sky is 
gray, and there's a cold wind blowing. The road stretches to the 
north and south.
|}

let house_desc = st {|
You see a small wooden shed off to the east.
|}

let rec road n here state ~first_time : State.t * Room.t =
  let add_room m (state:State.t) =
    let rooms = 
      Map.add state.rooms
        ~key:(Road m)
        ~data:(road m (Room.Road m)) 
    in
    { state with rooms }
  in
  if first_time then (
    print_endline road_desc;
    if n = 0 then (
      print_endline "";
      print_endline house_desc
    )
  );
  match prompt () with
  | Dir North -> (add_room (n + 1) state, Road (n + 1))
  | Dir South -> (add_room (n - 1) state, Road (n - 1))
  | Dir East when n = 0 -> (state,Shed)
  | Look "shed" when n = 0 ->
    sayf "It doesn't look like much from here. Maybe take a closer look?";
    (state,here)
  | ans -> 
    otherwise ans ~things:["road";"dirt"] state here

let () = register (Road 2) (road 2)

let shed_desc = st {|
You're standing in front of a gray shed with a rickety looking 
door. There's a small plaque to the right of the door, and there
are leaves and rocks scattered on the floor in front of the door.
|};;

let plaque_desc = st {|
Welcome intrepid adventurers! If you're here, then surely
you're interested in a life of awesome exploits and terrifying
danger that test your mettle. 

If so, then your first test is whether you can find a way past
the door.

(Note from the proprieters: it's not yet possible to get through
the door, but there will be soon.)
|}

let shed here (state:State.t) ~first_time : (_ * Room.t) =
  if first_time then (print_endline shed_desc);
  match prompt () with
  | Dir West -> (state, Road 0)
  | Look "plaque" ->
    sayf "It's a small bronze plaque, with intricate writing on it.";
    (state, here)
  | Read "plaque" ->
    print_endline plaque_desc;
    (state, here)
  | Open "door" ->
    if Set.mem state.inventory Rusty_key then (
      sayf "You put the key in the lock and turn. It grinds to the right and \n\
            you the door swings open.";
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
  | Look "leaves" ->
    sayf "You look at the leaves and see something glint.  Reaching down, \n\
          you see a small, rusty key, which you pick up.";
    let state = 
      { state with
        inventory = Set.add state.inventory Rusty_key }
    in
    (state, here)
  | ans -> 
    otherwise ans ~things:["shed";"door";"plaque";"leaves";"rocks"] state here

let () = register Shed shed


let inside_shed_desc = st {|
You stop inside the shed and notice that it seems a good bit bigger
on the inside than it did on the outside. You had to bend over to 
get inside, but as you stand up and look around you realize you're
in a large room with smooth granite walls.

There are torches on the walls, which cast a wavering orange light.
|}


let inside_shed here (state:State.t) ~first_time : (_ * Room.t) =
  if first_time then print_endline inside_shed_desc;
  otherwise (prompt ()) ~things:["wall";"walls";"torch";"torches"]
    state here

let () = register Inside_shed inside_shed


(** The runtime for executing the game *)

let rec run (state:State.t) ~old room =
  match Map.find state.rooms room with
  | None -> sayf "Huh. I'm lost. Game over."
  | Some f ->
    let first_time = not (Room.equal old room) in
    let (state',room') = f state ~first_time in
    run state' ~old:room room'

let () =
  run !state_ref ~old:Nowhere (Road 2)
