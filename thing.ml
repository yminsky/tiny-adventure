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

(* Terrible abuse of sexp converters to get printing and parsing on
   the cheap *)

let of_string s =
  let s = String.tr ~target:' ' ~replacement:'_' s in
  match t_of_sexp (Sexplib.Sexp.of_string s) with
  | x -> Some x
  | exception _ -> None

let to_string t =
  sexp_of_t t
  |> Sexplib.Sexp.to_string
  |> String.lowercase
  |> String.tr ~target:'_' ~replacement:' '
