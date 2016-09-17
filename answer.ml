open Import

type direction = 
  | North
  | South
  | East
  | West
type t =
  | Dir of direction
  | Take of string
  | Drop of string
  | Look_at of string
  | Look
  | Read of string
  | Open of string
  | Enter of string
  | Inventory
  | Other of string list
