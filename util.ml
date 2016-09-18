open Base

let printf = Printf.printf

let sayf format =
  Printf.ksprintf (fun s -> print_endline (String.strip s)) format

let prompt () =
  printf "\n>>> %!";
  match input_line stdin with
  | exception _ -> Other []
  | x ->
    let words = 
      let open List.Let_syntax in
      String.split (String.strip x) ~on:' '
      >>| String.strip
      >>| String.lowercase
      >>| String.filter ~f:Char.is_alphanum
    in
    Parser.run words

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

let singular s =
  match String.get s (String.length s - 1) with
  | 's' -> false
  | _ -> true
  | exception _ -> true
;;


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
      then (
        if singular s
        then sayf "It looks like a perfectly ordinary %s." s
        else sayf "They look like perfectly ordinary %s." s
      ) else printf "I see no %s here." s
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
  | Exit -> (state,Exit)
  | Other _ ->
    sayf "Sorry, I didn't understand that.";
    (state,here)
  end
;;
