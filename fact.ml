open Base

module T = struct
  type t =
    | Shed_door_is_open
    | Armory_junk_examined
  [@@deriving compare, sexp]       
end
include T
include Comparable.Make(T)
