open Base

val printf : ('a, out_channel, unit) format -> 'a

(* strip whitespace from a string *)
val st : string -> string

val sayf : ('a, unit, string, unit) format4 -> 'a

val prompt : unit -> Answer.t
