open Base
open Util

type room_f = t -> t * Room.t

and t = 
  { rooms        : room_f Map.M(Room).t
  ; room_things  : Set.M(Thing).t Map.M(Room).t
  ; inventory    : Set.M(Thing).t
  ; facts        : Set.M(Fact).t
  ; descriptions : (t -> string) Map.M(Room).t
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
    Sexplib.Sexp.of_string s
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
  | Game_over -> game_over ()
  | Exit -> ()
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
      run' t ~old:Nowhere room ~game_over
    | exception exn ->
      sayf "Well, that was unexpected. Your load failed. Here's what I got:";
      print_endline (Exn.to_string exn);
      run' t ~old room ~game_over
    end
  | _ -> 
    match Map.find t.rooms room with
    | None -> game_over ()
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
    run' start_state ~old:Nowhere start_room
      ~game_over:(make_game_over ~start_room ~start_state)
  | exception _ -> huh ()
  | _ -> huh ()
;;    

let run state room =
  run' state ~old:Nowhere room
    ~game_over:(make_game_over ~start_room:room ~start_state:state)
