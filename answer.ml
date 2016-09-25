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
[@@deriving variants]

(* @@deriving variants does something odd in the above case, so we
   need to fix it. *)
let open_ x = Open x
