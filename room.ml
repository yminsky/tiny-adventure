open Base

module T = struct
  type t =
    | Road of int
    | Shed
    | Inside_shed
    | Nowhere
  [@@deriving compare, sexp]
end
include T
include Comparable.Make(T)
