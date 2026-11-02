(* The downloads scenario, shared by several test files (the Rust port's
   tests/common/mod.rs): a [downloader] with Effects mounted twice ([/a],
   [/b]) sharing one download Service, and a [counter] leaf with no Effects. *)

open Causet

(* ---------------------------------------------------------------------- *)
(* Messages *)

type download_event =
  | Done of { request : Request_id.t; bytes : int }
  | Pushed of { note : string }

let download_events : download_event Message.t =
  Message.make ~name:"download.event"
    ~kind:(function Done _ -> "download.done" | Pushed _ -> "download.pushed")
    ~pp:(fun f -> function
      | Done { request; bytes } ->
          Format.fprintf f "Done { request: %a, bytes: %d }" Request_id.pp request bytes
      | Pushed { note } -> Format.fprintf f "Pushed { note: %S }" note)
    ()

(* Adapted by nobody: the downloader never registers it. *)
type ping = Ping

let ping : ping Message.t = Message.single "ping"

type downloader_action =
  | Fetch of string
  | Completed of { request : Request_id.t; bytes : int }
  | Finished of { url : string; bytes : int }
  | Probe
  | Note of int
  | Bad_reply
  | Pushed_note of string
  | Crash  (** Raises in the reducer. *)
  | Crash_in_handler  (** Its Effect dispatches [Note 7], then raises in the handler. *)
  | Explode  (** Raises in the enabling check. *)

let downloader_actions : downloader_action Message.t =
  Message.make ~name:"downloader.action"
    ~kind:(function
      | Fetch _ -> "downloader.fetch"
      | Completed _ -> "downloader.completed"
      | Finished _ -> "downloader.finished"
      | Probe -> "downloader.probe"
      | Note _ -> "downloader.note"
      | Bad_reply -> "downloader.bad_reply"
      | Pushed_note _ -> "downloader.pushed"
      | Crash -> "downloader.crash"
      | Crash_in_handler -> "downloader.crash_in_handler"
      | Explode -> "downloader.explode")
    ()

type downloader_effect =
  | Fetch_effect of { request : Request_id.t; url : string }
  | Probe_effect
  | Bad_reply_effect
  | Crash_in_handler_effect

let downloader_effects : downloader_effect Message.t =
  Message.make ~name:"downloader.effect"
    ~kind:(function
      | Fetch_effect _ -> "downloader.effect.fetch"
      | Probe_effect -> "downloader.effect.probe"
      | Bad_reply_effect -> "downloader.effect.bad_reply"
      | Crash_in_handler_effect -> "downloader.effect.crash_in_handler")
    ()

(* ---------------------------------------------------------------------- *)
(* The Service: a capability record. *)

type download_service = { fetch : Request_id.t -> string -> download_event reply -> unit }

(* Replies from a worker thread, optionally twice. *)
let threaded ~duplicate =
  {
    fetch =
      (fun request url reply ->
        let send () =
          match
            Reply.send_timeout reply
              (Done { request; bytes = String.length url })
              ~timeout:1.0
          with
          | Ok () -> ()
          | Error _ -> failwith "worker lane"
        in
        ignore
          (Thread.create
             (fun () ->
               send ();
               if duplicate then send ())
             ()));
  }

(* Replies synchronously, on the Store's thread. *)
let inline =
  {
    fetch =
      (fun request url reply ->
        match Reply.try_send reply (Done { request; bytes = String.length url }) with
        | Ok () -> ()
        | Error _ -> failwith "the Store lane is never full");
  }

(* ---------------------------------------------------------------------- *)
(* Downloader *)

type downloader_state = {
  pending : (Request_id.t * string) list;
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
    component =
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
          match List.assoc_opt request s.pending with
          | Some url ->
              Ctx.set_state ctx { s with pending = List.remove_assoc request s.pending };
              Ctx.emit_action ctx (Finished { url; bytes })
          | None -> Ctx.set_state ctx { s with stale = s.stale @ [ request ] })
      | Finished { url; bytes } ->
          Ctx.update ctx (fun s -> { s with done_ = s.done_ @ [ (url, bytes) ] })
      | Probe -> Ctx.emit_effect ctx Probe_effect
      | Note n -> Ctx.update ctx (fun s -> { s with notes = s.notes @ [ n ] })
      | Bad_reply -> Ctx.emit_effect ctx Bad_reply_effect
      | Pushed_note note ->
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
            failwith "Note 0 should be Disabled";
          if Effect_ctx.dispatch ctx (Note 1) <> Executed then
            failwith "Note 1 should be Executed"
      | Bad_reply_effect -> ignore (Effect_ctx.reply ctx ping)
      | Crash_in_handler_effect ->
          ignore (Effect_ctx.dispatch ctx (Note 7));
          failwith "handler crashed")
    ~register:(fun r ->
      Registrar.event r download_events (fun ev port ->
          match ev with
          | Done { request; bytes } ->
              Port.emit_action port (Completed { request; bytes })
          | Pushed { note } -> Port.emit_action port (Pushed_note note)))
    ()

(* ---------------------------------------------------------------------- *)
(* Counter: a leaf with no Effects, so no Service binding. *)

type counter_state = { n : int }
type inc = Inc

let incs : inc Message.t = Message.single "counter.inc"

let counter : (counter_state, unit, inc, never, never) component =
  Component.make ~name:"counter" ~actions:incs ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx Inc _ -> Ctx.update ctx (fun s -> { n = s.n + 1 }))
    ()

(* ---------------------------------------------------------------------- *)
(* App *)

type app_state = { a : downloader_state; b : downloader_state; counter : counter_state }
type app_services = { downloads : download_service }
type noop = Noop

let noops : noop Message.t = Message.single "app.noop"
let a_slot = Child.make "a" downloader
let b_slot = Child.make "b" downloader
let counter_slot = Child.make "counter" counter

let app : (app_state, app_services, noop, never, never) component =
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

(* ---------------------------------------------------------------------- *)
(* Fixture *)

type downloader_mount =
  (downloader_state, download_service, downloader_action, downloader_effect, never) mount

type fixture = {
  store : (app_state, app_services) Store.t;
  sink : Sink.t;
  trace : Observe.Trace_log.log;
  a : downloader_mount;
  b : downloader_mount;
  counter : (counter_state, unit, inc, never, never) mount;
}

let protocol () =
  let reg = Registry.create () in
  Registry.root reg app;
  match Registry.freeze reg with
  | Ok p -> p
  | Error problems -> failwith (String.concat "; " (List.map problem_message problems))

let fixture ?clock ?(state = empty_app) downloads worker_capacity =
  let proto = protocol () in
  let sink, inbox = Event_bus.create proto ~worker_capacity in
  let store = Store.create_exn ?clock proto ~state ~services:{ downloads } inbox in
  let trace = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer trace);
  let root = Path.root app in
  {
    store;
    sink;
    trace;
    a = Protocol.resolve_exn proto (Path.child root a_slot);
    b = Protocol.resolve_exn proto (Path.child root b_slot);
    counter = Protocol.resolve_exn proto (Path.child root counter_slot);
  }

let drain_until store done_ =
  let deadline = Unix.gettimeofday () +. 5.0 in
  while not (done_ (Store.state store)) do
    (match Store.drain_events store 64 with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "no fault");
    if Unix.gettimeofday () >= deadline then Alcotest.fail "timed out waiting for replies";
    Thread.delay 0.001
  done

(* ---------------------------------------------------------------------- *)
(* Helpers shared by the event tests *)

(* Runs [f] on a new thread and returns what it returned. *)
let on_thread f =
  let result = ref None in
  let t =
    Thread.create (fun () -> result := Some (try Ok (f ()) with e -> Error e)) ()
  in
  (t, result)

let join (t, result) =
  Thread.join t;
  match !result with
  | Some (Ok v) -> v
  | Some (Error e) -> raise e
  | None -> Alcotest.fail "the thread returned nothing"

let in_thread f = join (on_thread f)

let pp_send_error pp f = function
  | Full ev -> Format.fprintf f "Full (%a)" pp ev
  | Closed ev -> Format.fprintf f "Closed (%a)" pp ev
  | Wrong_protocol ev -> Format.fprintf f "Wrong_protocol (%a)" pp ev
  | Unregistered ev -> Format.fprintf f "Unregistered (%a)" pp ev

let send_result pp =
  Alcotest.testable
    (fun f -> function
      | Ok () -> Format.pp_print_string f "Ok ()"
      | Error e -> Format.fprintf f "Error (%a)" (pp_send_error pp) e)
    ( = )

let download_event = Alcotest.testable download_events.pp ( = )
let download_send_result = send_result download_events.pp

let drained =
  Alcotest.testable
    (fun f -> function
      | Ok n -> Format.fprintf f "Ok %d" n
      | Error (Faulted m) -> Format.fprintf f "Error (Faulted %S)" m
      | Error Foreign_mount -> Format.pp_print_string f "Error Foreign_mount"
      | Error Foreign_bus -> Format.pp_print_string f "Error Foreign_bus")
    ( = )

let dispatched =
  Alcotest.testable
    (fun f -> function
      | Ok Executed -> Format.pp_print_string f "Ok Executed"
      | Ok Disabled -> Format.pp_print_string f "Ok Disabled"
      | Ok Faulted -> Format.pp_print_string f "Ok Faulted"
      | Error (Faulted m) -> Format.fprintf f "Error (Faulted %S)" m
      | Error Foreign_mount -> Format.pp_print_string f "Error Foreign_mount"
      | Error Foreign_bus -> Format.pp_print_string f "Error Foreign_bus")
    ( = )

let ok_dispatch store mount action =
  Alcotest.check dispatched "dispatched" (Ok Executed)
    (Store.dispatch_to store mount action)
