open Base

module T = struct
  type road_spots = int
  [@@deriving compare, sexp]

  type special =
    | Game_over
    | Exit
    | Load
    | Save
    | Nowhere
  [@@deriving compare, sexp, enumerate]

  let all_of_road_spots = [2]

  type t =
    | Road of road_spots
    | Shed
    | Inside_shed
    | Corridor_1
    | Corridor_2
    | Dragon_lair
    | Exit_from_lair
    | Armory
    | Special of special
  [@@deriving compare, sexp, enumerate]
end
include T
include Comparable.Make(T)



(* A little map.

   DR-EX
   | 
   C1
   |
   IS-C2-AR
*)
