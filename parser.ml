open! Base

let run words : Answer.t =
  let c = String.concat ~sep:" " in
  match words with
  | ["go";"north"] | ["n"] | ["north"] -> Dir North
  | ["go";"south"] | ["s"] | ["south"] -> Dir South
  | ["go";"west"]  | ["w"] | ["west"]  -> Dir West
  | ["go";"east"]  | ["e"] | ["east"]  -> Dir East
  | "take" :: "the" :: x | "take" :: x -> Take (c x)
  | "drop" :: "the" :: x | "drop" :: x -> Drop (c x)
  | "read" :: "the" :: x | "read" :: x -> Read (c x)
  | "look" :: "at" :: "the" :: x | "look" :: "at" :: x -> Look_at (c x)
  | ["look"] | ["look";"around"] -> Look
  | "open" :: "the" :: x | "open" :: x -> Open (c x)
  | "enter" :: "the" :: x | "enter" :: x | "go" :: "in" :: x -> Enter (c x)
  | ["inventory"] | ["i"] -> Inventory
  | s -> Other s
