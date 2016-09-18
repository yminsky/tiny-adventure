type run_response = State.t * Room.t

module type Room_definition = sig
  val room : Room.t
  val desc : State.t -> string
  val run : Room.t -> State.t -> State.t * Room.t
  val things : Thing.t list
end

val state_of_rooms : (module Room_definition) list -> State.t

val drop : State.t -> Room.t -> string -> State.t * Room.t
val take : State.t -> Room.t -> string -> State.t * Room.t
val inventory : State.t -> 'a -> State.t * 'a
val first_help : string
val second_help_prelude : string
val third_help : string
val second_help : string
val singular : string -> bool
val otherwise :
  things:string list -> Answer.t -> State.t -> Room.t -> State.t * Room.t
