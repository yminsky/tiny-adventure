open Base

module T = struct
  type t = 
    | Rusty_key
    | Torch
    | Sword
    | Shield
  [@@deriving compare, sexp, enumerate]
end
include T
include Comparable.Make(T)

let of_string s =
  match s with
  | "rusty key" -> Some Rusty_key
  | "torch" -> Some Torch
  | "sword" -> Some Sword
  | "shield" -> Some Shield
  | _ -> None

let to_string = function
  | Rusty_key -> "rusty key"
  | Torch -> "torch"
  | Sword -> "sword"
  | Shield -> "shield"
