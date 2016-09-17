open Base

module T = struct
  type t = 
    | Rusty_key
  [@@deriving compare, sexp]
end
include T
include Comparable.Make(T)

let of_string s =
  match s with
  | "rusty key" -> Some Rusty_key
  | _ -> None

let to_string = function
  | Rusty_key -> "rusty key"
