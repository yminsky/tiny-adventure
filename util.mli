open Base

type run_response = State.t * Room.t

val printf : ('a, out_channel, unit) format -> 'a

val sayf : ('a, unit, string, unit) format4 -> 'a

val prompt : unit -> Answer.t

val otherwise :
  things:string list -> Answer.t -> State.t -> Room.t -> State.t * Room.t
