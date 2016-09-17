open Base

type room_f = t -> t * Room.t

and t = 
  { rooms : room_f Map.M(Room).t
  ; room_things : Set.M(Thing).t Map.M(Room).t
  ; inventory : Set.M(Thing).t
  ; facts : Set.M(Fact).t
  ; descriptions : (t -> string) Map.M(Room).t
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

let print_description (t:t) room =
  begin match Map.find t.descriptions room with
  | None -> ()
  | Some desc ->
    print_endline (desc t)
  end;
  match Map.find t.room_things room with
  | None -> ()
  | Some things ->
    if Set.is_empty things then ()
    else (
      print_newline ();
      Set.iter things ~f:(fun thing ->
        Util.sayf "You see a %s" (Thing.to_string thing))
    )

let rec run' t ~old room ~(game_over:unit -> unit) =
  if Room.equal room Game_over then game_over ()
  else match Map.find t.rooms room with
    | None -> game_over ()
    | Some f ->
      if not (Room.equal old room) then print_description t room;
      let (state',room') = f t in
      run' state' ~old:room room' ~game_over
;;

let rec game_over ~start_room ~start_state () =
  let huh () =
    Util.sayf "Huh?";
    game_over ~start_room ~start_state ()
  in
  print_newline ();
  Util.sayf "Game over. Would you like to play again?";
  match String.strip (String.lowercase (input_line stdin)) with
  | "n" | "no" -> ()
  | "y" | "yes" -> 
    run' start_state ~old:Nowhere start_room
      ~game_over:(game_over ~start_room ~start_state)
  | exception _ -> huh ()
  | _ -> huh ()
;;    

let run state room =
  run' state ~old:Nowhere room
    ~game_over:(game_over ~start_room:room ~start_state:state)
