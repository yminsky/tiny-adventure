open Base

module T = struct
  type road_spots = int
  [@@deriving compare, sexp]

  (* This is a bit of an abuse, since we're not listing all rooms
     here, but really just the rooms that we expect to have initialized
     at the start. *)
  let all_of_road_spots = [-2;0]

  (** Special rooms that are really messages to the runtime, rather
      than ordinary rooms. *)
  type special =
    | Exit
    | Game_over
    | Load
    | Nowhere
    | Save
  [@@deriving compare, sexp, enumerate]

  type t =
    | Special of special
    | Road of road_spots
    | Armory
    | Corridor_1
    | Corridor_2
    | Dragon_lair
    | Exit_from_lair
    | Inside_shed
    | Shed
  [@@deriving compare, sexp, enumerate]
end
include T
include Comparable.Make(T)
