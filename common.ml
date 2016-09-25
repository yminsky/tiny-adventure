open Base

let () = Random.self_init ()

type run_response = State.t * Room.t

let sayf = State.sayf

let prompt () =
  Printf.printf "\n>>> %!";
  match input_line stdin with
  | exception _ -> Other ""
  | x -> Parser.run x

let plural s =
  match String.get s (String.length s - 1) with
  | 's' -> true
  | _ -> false
  | exception _ -> false
;;

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

let take ~other_things state room thing_s = 
  let action =
    let open Option.Let_syntax in
    let%bind thing = Thing.of_string thing_s in
    State.take state room thing
  in
  match action with
  | Some state' -> (state',room)
  | None ->
    if Set.mem other_things thing_s 
    then (sayf "I can't take that.")
    else (
      if plural thing_s
      then (sayf "There are no %s for me to take." thing_s)
      else (sayf "There's no %s for me to take." thing_s)
    );
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
  let things =
    let stringify things =
      Set.map (module String) things ~f:Thing.to_string
    in
    let (++) = Set.union in
    Set.of_list (module String) things
    ++ stringify state.inventory
    ++ (Map.find state.room_things here
        |> Option.value ~default:(Set.empty (module Thing))
        |> stringify)
  in
  begin match ans with
  | Dir _ ->
    sayf "You can't go that way.";
    (state,here)
  | Take s -> take state here s ~other_things:things
  | Drop s -> drop state here s
  | Look ->
    State.print_description state here;
    (state,here)
  | Look_at (_,s) ->
    if Set.mem things s then begin
      if plural s
      then sayf "They look like perfectly ordinary %s." s
      else sayf "It looks like a perfectly ordinary %s." s
    end else begin 
      sayf "I see no %s here." s
    end;
    (state,here)
  | Read s ->
    if String.equal s "book" 
    then sayf {|
You think back to your copy of Land of Stories, and are sad
 you don't have it with you.|}
    else if Set.mem things s 
    then sayf "That is hardly a gripping read."
    else sayf "I don't see a %s worth reading." s;
    (state,here)
  | Move s ->
    if Set.mem things s then begin
      sayf "I don't see what that's going to accomplish"
    end else begin
      if plural s
      then sayf "I don't see a %s to move" s
      else sayf "I don't see any %s to move" s
    end;
    (state,here)
  | Open s ->
    (if Set.mem things s 
     then sayf "I don't know how to open that."
     else sayf "I don't see a %s to open." s);
    (state,here)
  | Enter None ->
    sayf "I'm not sure precisely where you're trying to go.";
    (state,here)
  | Enter (Some s) ->
    if Set.mem things s
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
  | Save -> (state,Special Save)
  | Load -> (state,Special Load)
  | Exit -> (state,Special Exit)
  | Other _ ->
    sayf "Sorry, I didn't understand that.";
    (state,here)
  end
;;
