type lamp_action = Dim of { percent : int } [@level Info] [@fields percent] | Unplug
[@@deriving message ~name:"lamp.action"]

type room_action = Vacate | Lamp of lamp_action [@nested]
[@@deriving message ~name:"room.action"]

let () =
  let dim = Lamp (Dim { percent = 40 }) in
  assert (room_action_message.name = "room.action");
  assert (room_action_message.kind Vacate = "room.vacate");
  (* The inner message's kind, level and fields. *)
  assert (room_action_message.kind dim = "lamp.dim");
  assert (room_action_message.level dim = Message.Info);
  assert (room_action_message.fields dim = [ ("percent", "40") ]);
  assert (Debug.to_string pp_room_action dim = "Lamp(Dim { percent: 40 })")
