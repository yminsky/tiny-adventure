open Base

type direction =
  | North
  | South
  | East
  | West

type how_to_look = At | Under

type t =
  | Dir of direction
  | Take of string
  | Drop of string
  | Look_at of how_to_look * string
  | Read of string
  | Open of string
  | Enter of string option
  | Help
  | Look
  | Inventory
  | Save
  | Load
  | Exit
  | Other of string list
