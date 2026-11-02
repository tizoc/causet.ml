(* The logger's records, as the Rust port's tests/observe.rs checks them: a
   child's Action reduced inline is stamped with its parent's time. *)

open Causet

(* The messages logged on [Causet_observers.src] while [f] runs. *)
let captured f =
  let lines = ref [] in
  let report src _level ~over k msgf =
    if Logs.Src.equal src Causet_observers.src then
      msgf (fun ?header:_ ?tags:_ fmt ->
          Format.kasprintf
            (fun s ->
              lines := !lines @ [ s ];
              over ();
              k ())
            fmt)
    else (
      over ();
      k ())
  in
  let reporter = Logs.reporter () in
  let level = Logs.Src.level Causet_observers.src in
  Logs.set_reporter { Logs.report };
  Logs.Src.set_level Causet_observers.src (Some Logs.Debug);
  Fun.protect
    ~finally:(fun () ->
      Logs.set_reporter reporter;
      Logs.Src.set_level Causet_observers.src level)
    f;
  !lines

let field name line =
  let prefix = name ^ "=" in
  List.find_map
    (fun word ->
      if String.starts_with ~prefix word then
        Some
          (String.sub word (String.length prefix)
             (String.length word - String.length prefix))
      else None)
    (String.split_on_char ' ' line)

type kid_action = Add of int

let kid_actions = Message.make ~name:"kid.action" ~kind:(fun (Add _) -> "kid.add") ()

let kid =
  Component.make ~name:"kid" ~actions:kid_actions ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx (Add n) _ -> Ctx.update ctx (fun s -> s + n))
    ()

let kid_slot = Child.make "kid" kid

type parent_action = Inline of int

let parent_actions =
  Message.make ~name:"parent.action" ~kind:(fun (Inline _) -> "parent.inline") ()

let parent =
  Component.make ~name:"parent" ~actions:parent_actions ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx (Inline n) _ -> ignore (Ctx.reduce_child ctx kid_slot (Add n)))
    ~register:(fun r ->
      Registrar.child r kid_slot ~lens:(Lens.field ~get:Fun.id ~set:(fun _ k -> k)) ())
    ()

let an_inline_call_is_logged_at_its_parent_s_time () =
  let call =
    {
      Observe.Inline_call.path = "/peers";
      instance = 1L;
      message = "peer.action";
      kind = "peer.dropped";
      within = 9;
      time = 4;
      root_guard_skipped = true;
    }
  in
  let logger = Causet_observers.logger () in
  let lines =
    captured (fun () -> logger.inline_action call (Message.payload kid_actions (Add 1)))
  in
  match lines with
  | [ line ] ->
      Alcotest.(check (list (option string)))
        "time, within, path, kind, root_guard_skipped"
        [ Some "4"; Some "9"; Some "/peers"; Some "peer.dropped"; Some "true" ]
        (List.map
           (fun f -> field f line)
           [ "time"; "within"; "path"; "kind"; "root_guard_skipped" ])
  | l -> Alcotest.failf "one record, got %d" (List.length l)

let a_store_s_inline_call_is_logged_at_its_parent_s_time () =
  let reg = Registry.create () in
  Registry.root reg parent;
  let proto =
    match Registry.freeze reg with Ok p -> p | Error _ -> failwith "invalid protocol"
  in
  let _sink, inbox = Event_bus.create proto ~worker_capacity:16 in
  let store =
    Store.create_exn
      ~clock:(Clock.Manual.clock (Clock.Manual.create 1_000))
      proto ~state:0 ~services:() inbox
  in
  Store.add_observer store (Causet_observers.logger ());
  let mount = Protocol.resolve_exn proto (Path.root parent) in
  let lines = captured (fun () -> ignore (Store.dispatch_exn store mount (Inline 2))) in
  match lines with
  | [ parent_line; inline_line ] ->
      Alcotest.(check (option string))
        "within" (field "id" parent_line) (field "within" inline_line);
      Alcotest.(check (option string)) "time" (Some "1000") (field "time" parent_line);
      Alcotest.(check (option string))
        "same time" (field "time" parent_line) (field "time" inline_line);
      (* Its mount has no root guard: the field is left out. *)
      Alcotest.(check (option string))
        "no root guard" None
        (field "root_guard_skipped" inline_line)
  | l -> Alcotest.failf "two records, got %d" (List.length l)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "logger"
    [
      ( "logger",
        [
          t "an inline call is logged at its parent's time"
            an_inline_call_is_logged_at_its_parent_s_time;
          t "a Store's inline call is logged at its parent's time"
            a_store_s_inline_call_is_logged_at_its_parent_s_time;
        ] );
    ]
