open Base

type t = 
  { rooms        : (t -> t * Room.t) Map.M(Room).t
  ; descriptions : (t -> string) Map.M(Room).t
  ; room_things  : Set.M(Thing).t Map.M(Room).t
  ; inventory    : Set.M(Thing).t
  ; facts        : Set.M(Fact).t
  }

let empty =
  { rooms        = Map.empty (module Room)
  ; room_things  = Map.empty (module Room)
  ; inventory    = Set.empty (module Thing)
  ; facts        = Set.empty (module Fact)
  ; descriptions = Map.empty (module Room)
  }

module Saveable = struct
  type full = t
  (* Sadly, the Saveable.t doesn't have quite everything you need to
     recover the world. In particular, the descriptions and rooms
     contain closures that can't be saved easily, and they're also
     allowed to change, specifically for self-modifying parts of the
     maze.

     The only solution right now is to try to make sure that the entry
     points to the self-modifying bits are there at startup. *)
  type t =
    { room_things  : Set.M(Thing).t Map.M(Room).t
    ; inventory    : Set.M(Thing).t
    ; facts        : Set.M(Fact).t
    ; room         : Room.t
    }
  [@@deriving sexp]

  let create ({ rooms = _
              ; room_things
              ; inventory
              ; facts
              ; descriptions = _
              } : full)
        room
    =
    ({ room_things; inventory; facts; room } : t)

  let inject ({room_things; inventory; facts; room}:t) (full:full) =
    ({ full with room_things; inventory; facts }, room)
end

let find_things_in_room t room =
  Map.find t.room_things room
  |> Option.value ~default:(Set.empty (module Thing))

let add_to_inventory t thing =
  { t with inventory = Set.add t.inventory thing }

let add_to_room t room thing =
  { t with 
    room_things = 
      Map.update t.room_things room ~f:(function
        | None -> Set.singleton (module Thing) thing
        | Some things -> Set.add things thing)
  }

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

let assert_fact t fact =
  { t with facts = Set.add t.facts fact }

let is_fact t fact =
  Set.mem t.facts fact

let sayf format =
  Printf.ksprintf (fun s -> print_endline (String.strip s)) format

let print_description (t:t) room =
  begin match Map.find t.descriptions room with
  | None -> ()
  | Some desc ->
    print_endline (String.strip (desc t))
  end;
  match Map.find t.room_things room with
  | None -> ()
  | Some things ->
    if Set.is_empty things then ()
    else (
      print_newline ();
      Set.iter things ~f:(fun thing ->
        sayf "You see a %s" (Thing.to_string thing))
    )

let save_filename = ".saved-game"

let save_exn t room =
  let saveable = 
    Saveable.create t room
    |> Saveable.sexp_of_t
    |> Sexplib.Sexp.to_string_hum
  in
  let file = open_out save_filename in
  output_string file saveable;
  output_string file "\n";
  close_out file
  
let load_exn t =
  let file = open_in save_filename in
  let bytes = in_channel_length file in
  let s = really_input_string file bytes in
  let saveable = 
    Sexplib.Sexp.of_string (String.strip s)
    |> Saveable.t_of_sexp
  in
  Saveable.inject saveable t

let rec run'
          t
          ~old
          (room:Room.t) 
          ~(game_over:unit -> unit)
  =
  match room with
  | Special special ->
    begin match special with
    | Game_over -> game_over ()
    | Exit | Nowhere -> ()
    | Save -> 
      begin match save_exn t old with
      | () -> 
        sayf "Your game has been saved";
        run' t ~old old ~game_over
      | exception exn ->
        sayf "Hmm. That didn't work. I'm not sure why. Here's the exception:";
        print_endline (Exn.to_string exn);
        run' t ~old old ~game_over
      end
    | Load ->
      begin match load_exn t with
      | (t,room) ->
        sayf "Old game loaded!";
        run' t ~old:(Special Nowhere) room ~game_over
      | exception exn ->
        sayf "Well, that was unexpected. Your load failed. Here's what I got:";
        print_endline (Exn.to_string exn);
        run' t ~old old ~game_over
      end
    end
  | _ -> 
    match Map.find t.rooms room with
    | None ->
      sayf {|
Odd. I can't seem to go there. Maybe you should report this to your 
Aba.|};
      run' t ~old old ~game_over
    | Some f ->
      if not (Room.equal old room) then print_description t room;
      let (state',room') = f t in
      run' state' ~old:room room' ~game_over
;;

let rec make_game_over ~start_room ~start_state () =
  let huh () =
    sayf "Huh?";
    make_game_over ~start_room ~start_state ()
  in
  print_newline ();
  sayf "Game over. Would you like to play again?";
  match String.strip (String.lowercase (input_line stdin)) with
  | "n" | "no" -> ()
  | "y" | "yes" -> 
    run' start_state ~old:(Special Nowhere) start_room
      ~game_over:(make_game_over ~start_room ~start_state)
  | exception _ -> huh ()
  | _ -> huh ()
;;    

let run state room =
  run' state ~old:(Special Nowhere) room
    ~game_over:(make_game_over ~start_room:room ~start_state:state)

(* Room generation *)

module type Room_definition = sig
  val here : Room.t
  val desc : t -> string
  val run : t -> t * Room.t
  val things : Thing.t list
end

let add_room t (module R : Room_definition) =
  let room_things = 
    match R.things with
    | [] -> t.room_things
    | things ->
      Map.add t.room_things ~key:R.here
        ~data:(Set.of_list (module Thing) things)
  in
  let rooms = Map.add t.rooms ~key:R.here ~data:R.run in
  let descriptions = Map.add t.descriptions ~key:R.here ~data:R.desc in
  { t with rooms; descriptions; room_things }

let of_rooms rooms =
  List.fold rooms ~init:empty ~f:add_room
