(* What the kettle can be asked to do. *)
type kettle_action = Switch_on of { target : int } | Switch_off
[@@deriving message ~name:"kettle.action"]

(* What it asks of its heater. *)
type kettle_effect = Heater_on | Heater_off [@@deriving message ~name:"kettle.effect"]

(* The thermometer's answer. *)
type temperature = { celsius : int } [@@deriving message ~name:"kettle.temperature"]

(* What it tells its parent. *)
type kettle_output = Boiled [@@deriving message ~name:"kettle.output"]

let () =
  assert (kettle_action_message.name = "kettle.action");
  assert (kettle_action_message.kind (Switch_on { target = 100 }) = "kettle.switch_on");
  assert (kettle_effect_message.kind Heater_off = "kettle.heater_off");
  assert (temperature_message.kind { celsius = 20 } = "kettle.temperature");
  assert (kettle_output_message.kind Boiled = "kettle.boiled")
