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
    | Look of string
    | Read of string
    | Open of string
    | Other of string list
end 

module Thing = struct
  module T = struct
    type t = 
      | Rusty_key
    [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make(T)
end

let parse : _ -> Answer.t = function
  | ["go";"north"] | ["n"] | ["north"] -> Dir North
  | ["go";"south"] | ["s"] | ["south"] -> Dir South
  | ["go";"west"]  | ["w"] | ["west"]  -> Dir West
  | ["go";"east"]  | ["e"] | ["east"]  -> Dir East
  | ["take";"the";x] | ["take";x] -> Take x
  | ["read";x] | [ "read"; "the"; x] -> Read x
  | ["look";"at";x] | [ "look"; "at";"the"; x] -> Look x
  | ["open";x] | ["open";"the";x] -> Open x
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
      | Nowhere
      | Here
    [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make(T)
end

let otherwise ~things (ans:Answer.t) state =
  begin match ans with
  | Dir _ ->
    sayf "You can't go that way."
  | Take s ->
    sayf "I don't see a %s to take." s
  | Look s ->
    if List.mem things s 
    then sayf "It looks like a perfectly ordinary %s." s
    else printf "I see no %s here." s
  | Read s ->
    if String.equal s "book" 
    then sayf "You think back to your copy of Land of Stories, and are sad \n\
               you don't have it with you."
    else if List.mem things s then
      sayf "That is hardly a gripping read."
    else
      sayf "I don't see a %s worth reading." s
  | Open s ->
    if List.mem things s 
    then sayf "I don't know how to open that."
    else sayf "I don't see a %s to open." s
  | Other ["help"] ->
    sayf "Oh, don't be such a baby. You can figure this out."
  | Other _ ->
    sayf "Sorry, I didn't understand that."
  end;
  (state,Room.Here)

type room_f = 
  state -> first_time:bool -> state * Room.t

and state = 
  { nothing : unit
  ; rooms : room_f Map.M(Room).t }

let state_ref =
  ref { rooms = Map.empty (module Room); nothing = () }

let register room room_f =
  state_ref := { !state_ref with 
                 rooms = Map.add (!state_ref).rooms ~key:room ~data:room_f 
               }

let road_desc = st {|
You are standing on the side of a deserted dirt road.  The sky is 
gray, and there's a cold wind blowing. The road stretches to the 
north and south.
|}

let house_desc = st {|
You see a small wooden shed off to the east.
|}


let rec road n state ~first_time : state * Room.t =
  let add_room m state =
    let rooms = Map.add state.rooms ~key:(Road m) ~data:(road m) in
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
    (state,Road n)
  | ans -> 
    otherwise ans ~things:["road";"dirt"] state

let () = register (Road 2) (road 2)

let shed_desc = st {|
You're standing in front of a gray shed with a rickety looking 
door. There's a small plaque to the right of the door.
|};;

let intro = st {|
Welcome intrepid adventurers! If you're here, then surely
you're interested in a life of awesome exploits and terrifying
danger that test your mettle. 

If so, then your first test is whether you can find a way past
the door.

(Note from the proprieters: it's not yet possible to get through
the door, but there will be soon.)
|}

let shed state ~first_time : (_ * Room.t) =
  if first_time then (print_endline shed_desc);
  match prompt () with
  | Dir West -> (state, Road 0)
  | Look "plaque" ->
    sayf "It's a small bronze plaque, with intricate writing on it.";
    (state, Here)
  | Read "plaque" ->
    print_endline intro;
    (state, Here)
  | Open "door" ->
    sayf "You try, but it's locked. It's surprisingly sturdy for \n\
          a shack that looks pretty beat up.";
    (state, Here)
  | ans -> 
    otherwise ans ~things:["shed";"door";"plaque"] state

let () = register Shed shed

(** FINAL code *)

let rec run state ~old room =
  match Map.find state.rooms room with
  | None -> sayf "Huh. I'm lost. Game over."
  | Some f ->
    let first_time = not (Room.equal old room) in
    let (state',room') = f state ~first_time in
    let room' = if Room.equal room' Here then room else room' in
    run state' ~old:room room'

let () =
  run !state_ref ~old:Nowhere (Road 2)
