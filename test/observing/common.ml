(* The downloads scenario, shared by several test files, ported from the Rust
   port's [tests/common/mod.rs]: a [downloader] with Effects mounted twice
   ([/a], [/b]) sharing one download Service, and a [counter] leaf with no
   Effects. Also the Rust-Debug-like renderings the tests compare. *)

open Causet

(* ------------------------------------------------------------------------ *)
(* Renderings, in the Rust port's Debug forms *)

let boundary_debug = function
  | Observe.Start -> "Start"
  | Observe.Finish -> "Finish"
  | Observe.Settled -> "Settled"
  | Observe.Bus -> "Bus"
  | Observe.Refused -> "Refused"
  | Observe.Inline -> "Inline"

let phase_debug = function
  | Observe.Admission -> "Admission"
  | Observe.Reducer -> "Reducer"
  | Observe.Handler -> "Handler"
  | Observe.Adapter -> "Adapter"
  | Observe.Clock -> "Clock"
  | Observe.Observer b -> "Observer(" ^ boundary_debug b ^ ")"
  | Observe.Invariant n -> Printf.sprintf "Invariant(%S)" n
  | Observe.Invariant_panic n -> Printf.sprintf "InvariantPanic(%S)" n

let by_debug = function
  | Observe.Root_guard -> "RootGuard"
  | Observe.Guard -> "Guard"
  | Observe.Component -> "Component"
  | Observe.Absent -> "Absent"

let sent_debug = function
  | Observe.Root -> "Root"
  | Observe.From_effect at -> Printf.sprintf "Effect(%d)" at
  | Observe.From_inline at -> Printf.sprintf "Inline(%d)" at
  | Observe.Queued o ->
      Printf.sprintf "Queued(causal %d, ordinal %d, lowering %d)" o.causal
        o.emission_ordinal o.lowering_ordinal

let at_debug = function None -> "None" | Some at -> Printf.sprintf "Some(%d)" at

let pp_request f (r : Request_id.t) =
  Format.fprintf f
    "RequestId { session: SessionId(%d), execution: ExecutionId(%d), index: %d }"
    r.session r.execution r.index

(* ------------------------------------------------------------------------ *)
(* Messages *)

type download_event =
  | Done of { request : Request_id.t; bytes : int }
  | Pushed of { note : string }

let download_events =
  Message.make ~name:"download.event"
    ~kind:(function Done _ -> "download.done" | Pushed _ -> "download.pushed")
    ~pp:(fun f -> function
      | Done { request; bytes } ->
          Format.fprintf f "Done { request: %a, bytes: %d }" pp_request request bytes
      | Pushed { note } -> Format.fprintf f "Pushed { note: %S }" note)
    ()

(* Adapted by the Downloader in the Rust port, but never registered. *)
type ping = Ping

let pings = Message.single ~pp:(fun f Ping -> Format.pp_print_string f "Ping") "ping"

type downloader_action =
  | Fetch of string
  | Completed of { request : Request_id.t; bytes : int }
  | Finished of { url : string; bytes : int }
  | Probe
  | Note of int
  | Bad_reply
  | Got_pushed of string  (** Rust's [DownloaderAction::Pushed]. *)
  | Crash  (** Raises in the reducer. *)
  | Crash_in_handler  (** Its Effect dispatches [Note 7], then raises in the handler. *)
  | Explode  (** Raises in the enabling check. *)

let downloader_actions =
  Message.make ~name:"downloader.action"
    ~kind:(function
      | Fetch _ -> "downloader.fetch"
      | Completed _ -> "downloader.completed"
      | Finished _ -> "downloader.finished"
      | Probe -> "downloader.probe"
      | Note _ -> "downloader.note"
      | Bad_reply -> "downloader.bad_reply"
      | Got_pushed _ -> "downloader.pushed"
      | Crash -> "downloader.crash"
      | Crash_in_handler -> "downloader.crash_in_handler"
      | Explode -> "downloader.explode")
    ~pp:(fun f -> function
      | Fetch url -> Format.fprintf f "Fetch(%S)" url
      | Completed { request; bytes } ->
          Format.fprintf f "Completed { request: %a, bytes: %d }" pp_request request bytes
      | Finished { url; bytes } ->
          Format.fprintf f "Finished { url: %S, bytes: %d }" url bytes
      | Probe -> Format.pp_print_string f "Probe"
      | Note n -> Format.fprintf f "Note(%d)" n
      | Bad_reply -> Format.pp_print_string f "BadReply"
      | Got_pushed note -> Format.fprintf f "Pushed(%S)" note
      | Crash -> Format.pp_print_string f "Crash"
      | Crash_in_handler -> Format.pp_print_string f "CrashInHandler"
      | Explode -> Format.pp_print_string f "Explode")
    ()

type downloader_effect =
  | Fetch_effect of { request : Request_id.t; url : string }
  | Probe_effect
  | Bad_reply_effect
  | Crash_in_handler_effect

let downloader_effects =
  Message.make ~name:"downloader.effect"
    ~kind:(function
      | Fetch_effect _ -> "downloader.effect.fetch"
      | Probe_effect -> "downloader.effect.probe"
      | Bad_reply_effect -> "downloader.effect.bad_reply"
      | Crash_in_handler_effect -> "downloader.effect.crash_in_handler")
    ~pp:(fun f -> function
      | Fetch_effect { request; url } ->
          Format.fprintf f "Fetch { request: %a, url: %S }" pp_request request url
      | Probe_effect -> Format.pp_print_string f "Probe"
      | Bad_reply_effect -> Format.pp_print_string f "BadReply"
      | Crash_in_handler_effect -> Format.pp_print_string f "CrashInHandler")
    ()

(* ------------------------------------------------------------------------ *)
(* The Service: a capability record. *)

type download_service = { fetch : Request_id.t -> string -> download_event reply -> unit }

(* Replies from a worker thread, optionally twice. *)
let threaded ~duplicate =
  {
    fetch =
      (fun request url reply ->
        ignore
          (Thread.create
             (fun () ->
               let done_ () = Done { request; bytes = String.length url } in
               let send () =
                 match Reply.send_timeout reply (done_ ()) ~timeout:1.0 with
                 | Ok () -> ()
                 | Error _ -> failwith "worker lane"
               in
               send ();
               if duplicate then send ())
             ()));
  }

(* Replies synchronously, on the Store's thread. *)
let inline_service =
  {
    fetch =
      (fun request url reply ->
        match Reply.try_send reply (Done { request; bytes = String.length url }) with
        | Ok () -> ()
        | Error _ -> failwith "the Store lane is never full");
  }

(* ------------------------------------------------------------------------ *)
(* Downloader *)

type downloader_state = {
  pending : (Request_id.t * string) list;  (** In request order, as a BTreeMap. *)
  done_ : (string * int) list;
  stale : Request_id.t list;
  notes : int list;
  pushed : string list;
}

let empty_downloader = { pending = []; done_ = []; stale = []; notes = []; pushed = [] }

let downloader :
    ( downloader_state,
      download_service,
      downloader_action,
      downloader_effect,
      never )
    Component.t =
  Component.make ~name:"downloader" ~actions:downloader_actions
    ~effects:downloader_effects ~outputs:Message.never
    ~is_enabled:(fun _ action _ ->
      match action with
      | Note 0 -> false
      | Explode -> failwith "admission exploded"
      | _ -> true)
    ~reduce:(fun ctx action _ ->
      match action with
      | Fetch url ->
          let request = Ctx.new_request ctx in
          Ctx.update ctx (fun s -> { s with pending = s.pending @ [ (request, url) ] });
          Ctx.emit_effect ctx (Fetch_effect { request; url })
      | Completed { request; bytes } -> (
          let s = Ctx.state ctx in
          let found = List.assoc_opt request s.pending in
          Ctx.set_state ctx { s with pending = List.remove_assoc request s.pending };
          match found with
          | Some url -> Ctx.emit_action ctx (Finished { url; bytes })
          | None -> Ctx.update ctx (fun s -> { s with stale = s.stale @ [ request ] }))
      | Finished { url; bytes } ->
          Ctx.update ctx (fun s -> { s with done_ = s.done_ @ [ (url, bytes) ] })
      | Probe -> Ctx.emit_effect ctx Probe_effect
      | Note n -> Ctx.update ctx (fun s -> { s with notes = s.notes @ [ n ] })
      | Bad_reply -> Ctx.emit_effect ctx Bad_reply_effect
      | Got_pushed note ->
          Ctx.update ctx (fun s -> { s with pushed = s.pushed @ [ note ] })
      | Crash -> failwith "downloader crashed"
      | Crash_in_handler -> Ctx.emit_effect ctx Crash_in_handler_effect
      | Explode -> failwith "unreachable: refused by its enabling check")
    ~handle:(fun ctx eff _ ->
      match eff with
      | Fetch_effect { request; url } ->
          let reply = Effect_ctx.reply ctx download_events in
          (Effect_ctx.service ctx).fetch request url reply
      | Probe_effect ->
          if Effect_ctx.dispatch ctx (Note 0) <> Disabled then
            failwith "assertion failed: Note(0) should be Disabled";
          if Effect_ctx.dispatch ctx (Note 1) <> Executed then
            failwith "assertion failed: Note(1) should be Executed"
      | Bad_reply_effect -> ignore (Effect_ctx.reply ctx pings)
      | Crash_in_handler_effect ->
          ignore (Effect_ctx.dispatch ctx (Note 7));
          failwith "handler crashed")
    ~register:(fun r ->
      Registrar.event r download_events (fun ev port ->
          match ev with
          | Done { request; bytes } ->
              Port.emit_action port (Completed { request; bytes })
          | Pushed { note } -> Port.emit_action port (Got_pushed note)))
    ()

(* ------------------------------------------------------------------------ *)
(* Counter: a leaf with no Effects, so no Service binding. *)

type counter_state = { n : int }
type inc = Inc

let incs =
  Message.make ~name:"counter.action"
    ~kind:(fun Inc -> "counter.inc")
    ~pp:(fun f Inc -> Format.pp_print_string f "Inc")
    ()

let counter : (counter_state, unit, inc, never, never) Component.t =
  Component.make ~name:"counter" ~actions:incs ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx Inc _ -> Ctx.update ctx (fun s -> { n = s.n + 1 }))
    ()

(* ------------------------------------------------------------------------ *)
(* App *)

type app_state = { a : downloader_state; b : downloader_state; counter : counter_state }
type app_services = { downloads : download_service }
type noop = Noop

let noops =
  Message.make ~name:"app.action"
    ~kind:(fun Noop -> "app.noop")
    ~pp:(fun f Noop -> Format.pp_print_string f "Noop")
    ()

let a_slot = Child.make "a" downloader
let b_slot = Child.make "b" downloader
let counter_slot = Child.make "counter" counter

let app : (app_state, app_services, noop, never, never) Component.t =
  Component.make ~name:"app" ~actions:noops ~effects:Message.never ~outputs:Message.never
    ~reduce:(fun _ Noop _ -> ())
    ~register:(fun r ->
      (* Both mounts project the same Service field: one shared Service. *)
      Registrar.child r a_slot
        ~lens:(Lens.field ~get:(fun s -> s.a) ~set:(fun s a -> { s with a }))
        ~service:(fun svc -> svc.downloads)
        ();
      Registrar.child r b_slot
        ~lens:(Lens.field ~get:(fun s -> s.b) ~set:(fun s b -> { s with b }))
        ~service:(fun svc -> svc.downloads)
        ();
      Registrar.child r counter_slot
        ~lens:
          (Lens.field
             ~get:(fun s -> s.counter)
             ~set:(fun s counter -> { s with counter }))
        ())
    ()

let empty_app = { a = empty_downloader; b = empty_downloader; counter = { n = 0 } }

(* ------------------------------------------------------------------------ *)
(* Fixture *)

type fixture = {
  store : (app_state, app_services) Store.t;
  sink : Sink.t;
  log : Observe.Trace_log.log;  (** The fixture's own trace, installed first. *)
  a :
    ( downloader_state,
      download_service,
      downloader_action,
      downloader_effect,
      never )
    mount;
  b :
    ( downloader_state,
      download_service,
      downloader_action,
      downloader_effect,
      never )
    mount;
  counter : (counter_state, unit, inc, never, never) mount;
}

let protocol () =
  let reg = Registry.create () in
  Registry.root reg app;
  match Registry.freeze reg with
  | Ok p -> p
  | Error problems -> failwith (String.concat "; " (List.map problem_message problems))

let fixture_from ?clock state downloads worker_capacity =
  let p = protocol () in
  let sink, inbox = Event_bus.create p ~worker_capacity in
  let store = Store.create_exn ?clock p ~state ~services:{ downloads } inbox in
  let log = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer log);
  let root = Path.root app in
  {
    store;
    sink;
    log;
    a = Protocol.resolve_exn p (Path.child root a_slot);
    b = Protocol.resolve_exn p (Path.child root b_slot);
    counter = Protocol.resolve_exn p (Path.child root counter_slot);
  }

let fixture_with ?clock downloads worker_capacity =
  fixture_from ?clock empty_app downloads worker_capacity

(* With the system clock, as Rust's [StoreOptions::default()]. *)
let fixture downloads worker_capacity = fixture_with downloads worker_capacity

let drain_until store is_done =
  let deadline = Unix.gettimeofday () +. 5.0 in
  while not (is_done (Store.state store)) do
    (match Store.drain_events store 64 with
    | Ok _ -> ()
    | Error _ -> failwith "no fault expected");
    if Unix.gettimeofday () >= deadline then failwith "timed out waiting for replies";
    Thread.delay 0.001
  done
