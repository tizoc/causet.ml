(* [@@deriving message]: type names, kinds, nested kinds and logging, ported
   from the Rust port's tests/derive.rs, and the printers, in Rust's Debug
   form. *)

open Causet

(* The messages carry fields only to be realistic; the tests read their kinds. *)
[@@@warning "-69"]

type cart_action = Clear [@@deriving message ~name:"cart.action"]

type shop_action =
  | Open
  | Add_item of string
  | Set_price of { cents : int }
  | Checkout [@kind "shop.checkout_started"]
  | Cart of cart_action [@nested]
[@@deriving message ~name:"shop.action"]

type shop_effect = Charge_card
[@@deriving message ~name:"shop.effect" ~prefix:"shop.effect"]

type paid = { cents : int } [@@deriving message ~name:"shop.paid"]
type sold = Sold [@kind "shop.sold"] [@@deriving message ~name:"shop.output"]

let kind m v = Message.kind (Message.payload m v)
let name m v = Message.name (Message.payload m v)

let enum_kinds_name_the_type_and_the_variant () =
  Alcotest.(check string) "type" "shop.action" shop_action_message.name;
  Alcotest.(check string) "open" "shop.open" (kind shop_action_message Open);
  Alcotest.(check string)
    "add_item" "shop.add_item"
    (kind shop_action_message (Add_item "tea"));
  Alcotest.(check string)
    "set_price" "shop.set_price"
    (kind shop_action_message (Set_price { cents = 1 }))

let overrides_win_over_the_defaults () =
  Alcotest.(check string)
    "checkout" "shop.checkout_started"
    (kind shop_action_message Checkout);
  Alcotest.(check string)
    "effect" "shop.effect.charge_card"
    (kind shop_effect_message Charge_card);
  Alcotest.(check string) "output" "shop.sold" (kind sold_message Sold)

let a_struct_kind_defaults_to_its_type () =
  Alcotest.(check string) "single" "shop.paid" (kind paid_message { cents = 1 })

let a_nested_variant_reports_the_inner_message_kind () =
  Alcotest.(check string) "nested" "cart.clear" (kind shop_action_message (Cart Clear))

let every_category_gets_its_marker () =
  Alcotest.(check (list string))
    "names"
    [ "shop.action"; "cart.action"; "shop.effect"; "shop.paid"; "shop.output" ]
    [
      name shop_action_message Open;
      name cart_action_message Clear;
      name shop_effect_message Charge_card;
      name paid_message { cents = 1 };
      name sold_message Sold;
    ]

(* What an observer sees of a message: the type's name and the value's kind. *)
let an_observer_sees_the_type_name_and_the_kind () =
  let seen = ref [] in
  let comp =
    Component.make ~name:"shop" ~actions:shop_action_message ~effects:shop_effect_message
      ~outputs:Message.never
      ~reduce:(fun ctx action _ ->
        match action with Checkout -> Ctx.emit_effect ctx Charge_card | _ -> ())
      ~handle:(fun _ Charge_card _ -> ())
      ()
  in
  let reg = Registry.create () in
  Registry.root reg comp;
  let proto = match Registry.freeze reg with Ok p -> p | Error _ -> failwith "freeze" in
  let _sink, inbox = Event_bus.create proto ~worker_capacity:4 in
  let store = Store.create_exn proto ~state:() ~services:() inbox in
  let record (e : Observe.Execution.t) p =
    seen := !seen @ [ Printf.sprintf "%s %s %s" e.message e.kind (Message.kind p) ]
  in
  Store.add_observer store
    { Observe.none with action_start = record; effect_start = record };
  let m = Protocol.resolve_exn proto (Path.root comp) in
  ignore (Store.dispatch_exn store m (Cart Clear));
  ignore (Store.dispatch_exn store m Checkout);
  Alcotest.(check (list string))
    "seen"
    [
      "shop.action cart.clear cart.clear";
      "shop.action shop.checkout_started shop.checkout_started";
      "shop.effect shop.effect.charge_card shop.effect.charge_card";
    ]
    !seen

(* ------------------------------------------------------------------------ *)
(* Logging: levels and fields. *)

type inner_action =
  | Failed of { code : int } [@level Error] [@fields code = display code]
[@@deriving message ~name:"inner.action"]

type peer_action =
  | Ping
  | Dropped of { peer : int; count : int; addr : string; secret : bytes } [@level Warn]
      [@fields peer, n = count, display addr]
  | Joined of { peer : int } [@fields peer]
  | Inner of inner_action [@nested]
[@@deriving message ~name:"peer.action" ~level:Trace]

type seen = { peer : int; at : int }
[@@deriving message ~name:"peer.seen" ~level:Info ~fields:peer]

let level m v = Message.log_level (Message.payload m v)
let fields m v = Message.log_fields (Message.payload m v)

let level_string = function
  | Message.Trace -> "trace"
  | Debug -> "debug"
  | Info -> "info"
  | Warn -> "warn"
  | Error -> "error"

let dropped =
  Dropped { peer = 7; count = 3; addr = "10.0.0.1"; secret = Bytes.of_string "\001" }

let log_levels_come_from_the_variant_then_the_type_then_debug () =
  let check what expected got =
    Alcotest.(check string) what expected (level_string got)
  in
  check "ping" "trace" (level peer_action_message Ping);
  check "dropped" "warn" (level peer_action_message dropped);
  check "joined" "trace" (level peer_action_message (Joined { peer = 1 }));
  check "seen" "info" (level seen_message { peer = 1; at = 2 });
  check "the default" "debug" (level shop_action_message Open)

let log_fields_are_the_selected_ones () =
  let check = Alcotest.(check (list (pair string string))) in
  check "dropped"
    [ ("peer", "7"); ("n", "3"); ("addr", "10.0.0.1") ]
    (fields peer_action_message dropped);
  check "joined" [ ("peer", "1") ] (fields peer_action_message (Joined { peer = 1 }));
  check "ping" [] (fields peer_action_message Ping);
  check "seen" [ ("peer", "4") ] (fields seen_message { peer = 4; at = 9 });
  check "the default" [] (fields shop_action_message (Add_item "tea"))

let a_nested_variant_logs_as_its_inner_message () =
  let failed = Inner (Failed { code = 503 }) in
  Alcotest.(check string)
    "level" "error"
    (level_string (level peer_action_message failed));
  Alcotest.(check (list (pair string string)))
    "fields"
    [ ("code", "503") ]
    (fields peer_action_message failed)

(* ------------------------------------------------------------------------ *)
(* Kinds in Rust's snake case, and printers in its Debug form. *)

(* Both spellings of a constructor give the Rust kind. *)
type spellings = CheckedOut | Checked_out | HTTPGet | X
[@@deriving message ~name:"s.action"]

let constructors_take_rusts_snake_case () =
  Alcotest.(check (list string))
    "kinds"
    [ "s.checked_out"; "s.checked_out"; "s.h_t_t_p_get"; "s.x" ]
    (List.map (kind spellings_message) [ CheckedOut; Checked_out; HTTPGet; X ])

module Id = struct
  type t = int

  let pp f n = Format.fprintf f "Id(%d)" n
end

type note = {
  text : string;
  tags : string list;
  id : Id.t;
  ratio : float;
  pair : int * bool option;
}
[@@deriving message ~name:"note"]

type t = Plain | Wrapped of note * char | Many of { notes : note array; on : bool }
[@@deriving message ~name:"t.action"]

type callback = { run : unit -> unit } [@@deriving message ~name:"callback" ~no_pp]

(* A trailing underscore escapes a keyword, and is not Rust's name. *)
type transfer = { to_ : int } [@@deriving debug]

(* Fields named as the generated code's own variables were ([f], the
   formatter; [x0], a tuple's first item; [inner]), as a printer it calls
   ([pp_note] prints a [note]), as a function it calls ([string_of_int])
   and as its variables are now: none of them is captured. *)
type clash = {
  f : int;
  x : string;
  x0 : bool;
  inner : int;
  pp_note : note;
  string_of_int : int;
  __causet_fmt : int;
}
[@@deriving
  message ~name:"clash" ~fields:(f, display x, display string_of_int, pp_note, inner)]

type clash_action =
  | Hit of { f : int; pp_note : note; __causet_value : int } [@fields f, display f]
  | Pair of clash * int
  | Wrap of clash [@nested]
[@@deriving message ~name:"clash.action"]

(* A printer written by hand: it may name the type's constructors, and the
   message prints with it, as does a type that holds this one. *)
type light = Red | Green
[@@deriving
  message ~name:"light" ~pp:(fun f l ->
      Format.pp_print_string f (match l with Red -> "R" | Green -> "G"))]

type crossing = { light : light; lanes : int } [@@deriving debug]

(* A type nothing prints, held by another: [~pp:Debug.opaque] makes the
   [pp_handle] the holder needs, where [~no_pp] would make none. *)
type handle = { close : unit -> unit }
[@@deriving message ~name:"handle" ~pp:Debug.opaque]

type holder = { handle : handle; id : int } [@@deriving debug]

(* No values: no constructor to print or to tell the kind of. *)
type nothing = | [@@deriving message ~name:"nothing"]

(* The signatures the deriver gives: with a printer, and with [~no_pp]
   none. *)
module Sig : sig
  type shown = Shown [@@deriving message]
  type unshown = Unshown [@@deriving message ~no_pp]
end = struct
  type shown = Shown [@@deriving message ~name:"sig.shown" ~pp:Debug.opaque]
  type unshown = Unshown [@@deriving message ~name:"sig.unshown" ~no_pp]
end

let show pp v = Debug.to_string pp v

let printers_print_as_rusts_debug () =
  let check = Alcotest.(check string) in
  let n =
    {
      text = "say \"hi\"\n";
      tags = [ "a"; "b" ];
      id = 4;
      ratio = 2.0;
      pair = (1, Some true);
    }
  in
  let n_shown =
    {|Note { text: "say \"hi\"\n", tags: ["a", "b"], id: Id(4), ratio: 2.0, pair: (1, Some(true)) }|}
  in
  check "record" n_shown (show pp_note n);
  check "constant" "Plain" (show pp Plain);
  check "tuple" ("Wrapped(" ^ n_shown ^ ", 'x')") (show pp (Wrapped (n, 'x')));
  check "inline record"
    ("Many { notes: [" ^ n_shown ^ "], on: false }")
    (show pp (Many { notes = [| n |]; on = false }));
  check "the message's" "SetPrice { cents: 5 }"
    (show shop_action_message.pp (Set_price { cents = 5 }));
  check "nested" "Cart(Clear)" (show pp_shop_action (Cart Clear));
  check "t gives message" "t.action" message.name;
  check "bytes" {|Dropped { peer: 7, count: 3, addr: "10.0.0.1", secret: [1] }|}
    (show pp_peer_action dropped);
  check "keyword field" "Transfer { to: 3 }" (show pp_transfer { to_ = 3 });
  check "no printer" "_" (show callback_message.pp { run = ignore });
  check "hand-written" "R" (show light_message.pp Red);
  check "hand-written, held" "Crossing { light: G, lanes: 2 }"
    (show pp_crossing { light = Green; lanes = 2 });
  check "opaque, held" "Holder { handle: _, id: 3 }"
    (show pp_holder { handle = { close = ignore }; id = 3 });
  check "signature" "_" (show Sig.shown_message.pp Sig.Shown);
  check "signature, no printer" "_" (show Sig.unshown_message.pp Sig.Unshown);
  ignore (nothing_message : nothing Message.t);
  Alcotest.(check (list string))
    "floats" [ "0.1"; "-3.5"; "1e+20" ]
    (List.map (show Debug.float) [ 0.1; -3.5; 1e20 ])

let no_field_captures_the_generated_code () =
  let note = { text = "n"; tags = []; id = 1; ratio = 0.5; pair = (2, None) } in
  let note_shown =
    {|Note { text: "n", tags: [], id: Id(1), ratio: 0.5, pair: (2, None) }|}
  in
  let c =
    {
      f = 1;
      x = "a";
      x0 = true;
      inner = 2;
      pp_note = note;
      string_of_int = 3;
      __causet_fmt = 4;
    }
  in
  let c_shown =
    "Clash { f: 1, x: \"a\", x0: true, inner: 2, pp_note: " ^ note_shown
    ^ ", string_of_int: 3, __causet_fmt: 4 }"
  in
  let check = Alcotest.(check string) in
  check "record" c_shown (show pp_clash c);
  check "message" c_shown (show clash_message.pp c);
  Alcotest.(check (list (pair string string)))
    "record fields"
    [
      ("f", "1");
      ("x", "a");
      ("string_of_int", "3");
      ("pp_note", note_shown);
      ("inner", "2");
    ]
    (fields clash_message c);
  let hit = Hit { f = 5; pp_note = note; __causet_value = 6 } in
  check "inline record"
    ("Hit { f: 5, pp_note: " ^ note_shown ^ ", __causet_value: 6 }")
    (show pp_clash_action hit);
  Alcotest.(check (list (pair string string)))
    "inline fields"
    [ ("f", "5"); ("f", "5") ]
    (fields clash_action_message hit);
  check "tuple" ("Pair(" ^ c_shown ^ ", 7)") (show pp_clash_action (Pair (c, 7)));
  check "nested" "clash" (kind clash_action_message (Wrap c));
  Alcotest.(check (list (pair string string)))
    "nested fields" (fields clash_message c)
    (fields clash_action_message (Wrap c))

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "derive"
    [
      ( "derive",
        [
          t "enum kinds name the type and the variant"
            enum_kinds_name_the_type_and_the_variant;
          t "overrides win over the defaults" overrides_win_over_the_defaults;
          t "a struct kind defaults to its type" a_struct_kind_defaults_to_its_type;
          t "a nested variant reports the inner message kind"
            a_nested_variant_reports_the_inner_message_kind;
          t "every category gets its marker" every_category_gets_its_marker;
          t "an observer sees the type name and the kind"
            an_observer_sees_the_type_name_and_the_kind;
          t "log levels come from the variant then the type then debug"
            log_levels_come_from_the_variant_then_the_type_then_debug;
          t "log fields are the selected ones" log_fields_are_the_selected_ones;
          t "a nested variant logs as its inner message"
            a_nested_variant_logs_as_its_inner_message;
          t "constructors take Rust's snake case" constructors_take_rusts_snake_case;
          t "printers print as Rust's Debug" printers_print_as_rusts_debug;
          t "no field captures the generated code" no_field_captures_the_generated_code;
        ] );
    ]
