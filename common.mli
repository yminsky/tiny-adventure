open Base

(** Some definitions that are useful in a variety of different rooms *)

type run_response = State.t * Room.t

val sayf : ('a, unit, string, unit) format4 -> 'a

val prompt : unit -> Answer.t

val otherwise :
  things:string list -> Answer.t -> State.t -> Room.t -> State.t * Room.t
