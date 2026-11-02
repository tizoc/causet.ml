The causet guide's OCaml examples for its messages page, run by mdx
(`dune test doc`). The guide's pages are in its own repository and show
each block here by the name above it.

<!-- snippet: declare -->
<!-- $MDX file=examples/messages_kettle.ml -->
```ocaml
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
```

<!-- snippet: kinds -->
<!-- $MDX file=examples/messages_kinds.ml -->
```ocaml
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
```

<!-- snippet: logging -->
<!-- $MDX file=examples/messages_door.ml -->
```ocaml
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
```

<!-- snippet: nested -->
<!-- $MDX file=examples/messages_room.ml -->
```ocaml
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
```

<!-- snippet: printers -->
<!-- $MDX file=examples/messages_tea.ml -->
```ocaml
(* A field of type [cup_id] is printed by [pp_cup_id]: [@@deriving debug]
   makes it. *)
type cup_id = Cup_id of int [@@unboxed] [@@deriving debug]

(* A field of type [Millilitres.t] by [Millilitres.pp], written here with
   [Debug], which prints as Rust's [Debug] does. *)
module Millilitres = struct
  type t = int

  let pp f n = Debug.tuple f "Millilitres" [ (fun f -> Debug.int f n) ]
end

type tea_action =
  | Pour of { cup : cup_id; amount : Millilitres.t }
  | Rinse of cup_id list
[@@deriving message ~name:"tea.action"]

(* The Effect that carries out a Pour is named apart, as one OCaml module
   needs, and keeps the Rust port's kind and printed name. *)
type tea_effect =
  | Pour_effect of { cup : cup_id } [@kind "tea.pour"] [@debug_name "Pour"]
[@@deriving message ~name:"tea.effect"]

let () =
  let show pp v = Debug.to_string pp v in
  assert (
    show pp_tea_action (Pour { cup = Cup_id 1; amount = 250 })
    = "Pour { cup: CupId(1), amount: Millilitres(250) }");
  assert (
    show pp_tea_action (Rinse [ Cup_id 1; Cup_id 2 ]) = "Rinse([CupId(1), CupId(2)])");
  assert (
    show tea_effect_message.pp (Pour_effect { cup = Cup_id 1 }) = "Pour { cup: CupId(1) }")
```

<!-- snippet: by-hand -->
```ocaml
open Causet

(* A thermometer tick, too frequent to log above trace. *)
type tick = { reading : int }

let tick_message : tick Message.t =
  Message.single
    ~level:(fun _ -> Message.Trace)
    ~pp:(fun f t -> Debug.record f "Tick" [ ("reading", fun f -> Debug.int f t.reading) ])
    "kettle.tick"

let () =
  let tick = { reading = 21 } in
  assert (tick_message.kind tick = "kettle.tick");
  assert (tick_message.level tick = Message.Trace);
  assert (Debug.to_string tick_message.pp tick = "Tick { reading: 21 }")
```
