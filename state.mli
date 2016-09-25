open! Base

type t = 
  { rooms        : (t -> t * Room.t) Map.M(Room).t
  ; descriptions : (t -> string) Map.M(Room).t
  ; room_things  : Set.M(Thing).t Map.M(Room).t
  ; inventory    : Set.M(Thing).t
  ; facts        : Set.M(Fact).t
  }

val find_things_in_room : t -> Room.t -> Set.M(Thing).t
val take : t -> Room.t -> Thing.t -> t option
val drop : t -> Room.t -> Thing.t -> t option
val assert_fact : t -> Fact.t -> t
val is_fact : t -> Fact.t -> bool

val print_description : t -> Room.t -> unit

(** Executes the game *)
val run : t -> Room.t -> unit

module type Room_definition = sig
  (** The name of the room in question *)
  val here : Room.t

  (** Returns a description that is shown when you arrive in a room
      and when the player looks around. Can depend on the state. *)
  val desc : t -> string

  (** Run the specific room, parsing input from the user, and finally
      returning the new state and the new room to go to.  *)
  val run : t -> t * Room.t

  (** The initial set of things in a room. *)
  val things : Thing.t list
end

(** builds up an initial state based on the provided rooms *)
val of_rooms : (module Room_definition) list -> t


(** We export this here so it doesn't need to be defined again in
    Util *)
val sayf : ('a, unit, string, unit) format4 -> 'a
