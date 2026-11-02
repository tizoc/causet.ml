type door_action =
  | Ring  (** At info, the type's level, with no fields. *)
  | Unlock of { name : string; code : string } [@level Warn] [@fields who = display name]
      (** At warn, with the person's name by its display form, and never the code. *)
[@@deriving message ~name:"door.action" ~level:Info]

(* At trace, with whether the door is ajar, by its printer. *)
type door_sensor = { ajar : bool; battery : int }
[@@deriving message ~name:"door.sensor" ~level:Trace ~fields:ajar]

let () =
  let unlock = Unlock { name = "Ada"; code = "1234" } in
  assert (door_action_message.level Ring = Message.Info);
  assert (door_action_message.level unlock = Message.Warn);
  assert (door_action_message.fields unlock = [ ("who", "Ada") ]);
  let sensor = { ajar = true; battery = 80 } in
  assert (door_sensor_message.level sensor = Message.Trace);
  assert (door_sensor_message.fields sensor = [ ("ajar", "true") ])
