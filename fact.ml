open Base

module T = struct
  type t =
    | Shed_door_is_open
    | Armory_junk_examined
    | Dragon_is_dead
    | Asked_for_help
  [@@deriving compare, sexp]       
end
include T
include Comparable.Make(T)
