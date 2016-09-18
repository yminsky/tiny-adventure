open Base

module T = struct
  type t =
    | Nowhere
    | Road of int
    | Shed
    | Inside_shed
    | Corridor_1
    | Corridor_2
    | Dragon_lair
    | Exit_from_lair
    | Armory
    | Game_over
    | Exit
    | Load
    | Save
  [@@deriving compare, sexp]
end
include T
include Comparable.Make(T)



(*
   DR-EX
   | 
   C1
   |
   IS-C2-AR
*)
