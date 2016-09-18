open Import

let room = Room.Armory
let things : Thing.t list = [Sword]

let desc _ = {|
This is clearly an armory, which is to say a place where weapons were
once stored. You can tell because of the racks on the walls that
clearly once held pikes and swords and the like.

But whatever it once was, it's now in disarray. There are piles of
junk everywhere, mostly bits of wood and cloth that probably had some
practical purpose once upon a time.
|}

let run here (state:State.t) : run_response =
  match prompt () with
  | Dir West -> (state,Corridor_2)
  | Look_at (Under,("junk"|"wood")) -> 
    if State.is_fact state Armory_junk_examined then (
      sayf {|
Yawn. Your further examination of the junk bores you to tears.|};
      (state,here)
    ) else (
      sayf {|
You look under the piles of wood, and notice a stout-looking, round
wooden shield. You pick it up for a moment, but, surprised by how
light it is, you let the shield tumble out of your hands. |};
      let state =
        { state with
          facts = Set.add state.facts Armory_junk_examined
        ; room_things =
            Map.update state.room_things here ~f:(fun set_opt ->
              let set = 
                Option.value ~default:(Set.empty (module Thing))
                  set_opt
              in
              Set.add set Shield)
        }
      in
      (state,here)
    )
  | ans ->
    otherwise ~things:["racks";"wood";"cloth";"junk"]
      ans state here


