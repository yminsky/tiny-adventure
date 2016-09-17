open Base

val printf : ('a, out_channel, unit) format -> 'a

val sayf : ('a, unit, string, unit) format4 -> 'a

val prompt : unit -> Answer.t
