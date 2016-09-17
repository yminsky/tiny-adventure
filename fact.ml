open Base

module T = struct
  type t =
    | Shed_door_is_open
  [@@deriving compare, sexp]       
end
include T
include Comparable.Make(T)
