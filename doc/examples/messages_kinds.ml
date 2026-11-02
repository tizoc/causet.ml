type heater =
  | Warm (* the prefix and the constructor *)
  | Cut [@kind "kettle.heater.off"] (* given *)
[@@deriving message ~name:"kettle.heater" ~prefix:"kettle.heater"]

(* A record has one kind: its own, here, rather than the type's name. *)
type dry_alarm = { dry_secs : int }
[@@deriving message ~name:"kettle.alarm" ~kind:"kettle.dry"]

let () =
  assert (heater_message.kind Warm = "kettle.heater.warm");
  assert (heater_message.kind Cut = "kettle.heater.off");
  assert (dry_alarm_message.kind { dry_secs = 3 } = "kettle.dry")
