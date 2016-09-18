(** This module is responsible for putting together the final
    assemblage of rooms. *)
open! Base

module type Room_definition = sig
  val here : Room.t
  val desc : State.t -> string
  val run : State.t -> State.t * Room.t
  val things : Thing.t list
end

val state_of_rooms : (module Room_definition) list -> State.t
