open Base

type direction =
  | North
  | South
  | East
  | West

type how_to_look = At | Under

type t =
  | Dir of direction
  | Drop of string
  | Enter of string option
  | Look_at of how_to_look * string
  | Move of string
  | Open of string
  | Read of string
  | Take of string

  | Help
  | Look
  | Inventory
  | Save
  | Load
  | Exit

  | Other of string

let drop x = Drop x
let enter x = Enter x
let look_at (x,y) = Look_at (x,y)
let move x = Move x
let open_ x = Open x
let read x = Read x
let take x = Take x
                             
