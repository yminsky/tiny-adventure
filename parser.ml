open! Base

(** Convert input into lowercase, with non alphanumeric characters
    dropped, and excess whitespace removed. *)
let normalize s = 
  let words =
    let open List.Let_syntax in
    String.split s ~on:' '
    >>| String.strip
    >>| String.lowercase
    >>= (fun s -> if String.(=) s "" then [] else [s])
    >>| String.filter ~f:Char.is_alphanum
  in
  String.concat ~sep:" " words

let starts_with_gen
      prefixes
      (make_answer : string option -> Answer.t option)
      input
  =
  List.find_map prefixes ~f:(fun prefix ->
    match String.chop_prefix ~prefix input with
    | None -> None
    | Some suffix ->
      let suffix = String.strip suffix in
      if String.(=) suffix "" 
      then make_answer None
      else make_answer (Some suffix)
  )
;;

let starts_with prefixes make_answer input =
  let make_answer = function
    | None -> None
    | Some x -> Some (make_answer x)
  in
  starts_with_gen prefixes make_answer input
;;

let is targets (answer:Answer.t) s =
  if List.exists targets ~f:(String.(=) s)
  then Some answer
  else None
;;

let parsers =
  [ is ["go north" ; "north" ; "n"] (Dir North)
  ; is ["go south" ; "south" ; "s"] (Dir South)
  ; is ["go west"  ; "west"  ; "w"] (Dir West)
  ; is ["go east"  ; "east"  ; "e"] (Dir East)

  ; starts_with ["take the";"take"]  Answer.take
  ; starts_with ["drop the";"drop"]  Answer.drop
  ; starts_with ["read the";"read"]  Answer.read
  ; starts_with [ "move the";"move"] Answer.move

  ; is ["look"; "look around"] Look

  ; is ["save"] Save
  ; is ["load"] Load

  ; starts_with ["open the";"open"; "unlock the";"unlock"] Answer.open_
  ; starts_with ["enter the";"enter";"go in"] (fun obj -> Enter (Some obj))
  ; is ["enter";"go in"] (Enter None)

  ; starts_with [ "look at the" ; "look at" ] (fun obj -> Look_at (At, obj))

  ; is ["inventory";"i"]  Inventory
  ; is ["help";"help me"] Help
  ; is ["quit";"exit"]    Exit
  ]

let run line =
  let line = normalize line in
  let result =
    List.find_map parsers ~f:(fun parse -> parse line)
  in
  match result with
  | Some x -> x
  | None -> Other line
