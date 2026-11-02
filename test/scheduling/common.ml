(* The downloads scenario of the Rust tests' [common] module, as far as these
   tests use it: a [downloader] with Effects mounted twice ([/a], [/b])
   sharing one download Service, and a [counter] leaf with no Effects. Also
   a few check helpers shared by the tests of this directory. *)

open Causet

(* ------------------------------------------------------------------------ *)
(* Checks *)

(* [expected = actual], printed with [pp] when they differ. *)
let eq ?(pp = fun f _ -> Format.pp_print_string f "<value>") name expected actual =
  Alcotest.check (Alcotest.testable pp ( = )) name expected actual

let pp_list pp f l =
  Format.fprintf f "[%a]"
    (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ") pp)
    l

let pp_outcome f = function
  | Executed -> Format.pp_print_string f "Executed"
  | Disabled -> Format.pp_print_string f "Disabled"
  | Faulted -> Format.pp_print_string f "Faulted"

let pp_dispatch_error f = function
  | Foreign_mount -> Format.pp_print_string f "Foreign_mount"
  | Foreign_bus -> Format.pp_print_string f "Foreign_bus"
  | Faulted m -> Format.fprintf f "Faulted %S" m

let pp_dispatch f = function
  | Ok o -> Format.fprintf f "Ok %a" pp_outcome o
  | Error e -> Format.fprintf f "Error (%a)" pp_dispatch_error e

let check_dispatch name expected actual = eq ~pp:pp_dispatch name expected actual

let freeze_exn reg =
  match Registry.freeze reg with
  | Ok p -> p
  | Error problems -> failwith (String.concat "; " (List.map problem_message problems))

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  go 0

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
          Format.fprintf f "Done { request: %a, bytes: %d }" Request_id.pp request bytes
      | Pushed { note } -> Format.fprintf f "Pushed { note: %S }" note)
    ()

(* Adapted by nobody: never registered. *)
type ping = Ping

let ping_events = Message.make ~name:"ping" ~kind:(fun Ping -> "ping") ()

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

let downloader_actions =
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

let downloader_effects =
  Message.make ~name:"downloader.effect"
    ~kind:(function
      | Fetch_effect _ -> "downloader.effect.fetch"
      | Probe_effect -> "downloader.effect.probe"
      | Bad_reply_effect -> "downloader.effect.bad_reply"
      | Crash_in_handler_effect -> "downloader.effect.crash_in_handler")
    ()

(* ------------------------------------------------------------------------ *)
(* The Service: a capability, held as a record of functions. *)

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
               (match Reply.send_timeout reply (done_ ()) ~timeout:1.0 with
               | Ok () -> ()
               | Error _ -> failwith "worker lane");
               if duplicate then
                 match Reply.send_timeout reply (done_ ()) ~timeout:1.0 with
                 | Ok () -> ()
                 | Error _ -> failwith "worker lane")
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

(* ------------------------------------------------------------------------ *)
(* Downloader *)

type downloader_state = {
  pending : (Request_id.t * string) list;  (** Ordered by request. *)
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
          Ctx.update ctx (fun s ->
              {
                s with
                pending =
                  List.sort
                    (fun (a, _) (b, _) -> Request_id.compare a b)
                    ((request, url) :: s.pending);
              });
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
      | Explode -> failwith "refused by its enabling check")
    ~handle:(fun ctx eff _ ->
      match eff with
      | Fetch_effect { request; url } ->
          let reply = Effect_ctx.reply ctx download_events in
          (Effect_ctx.service ctx).fetch request url reply
      | Probe_effect ->
          if Effect_ctx.dispatch ctx (Note 0) <> Disabled then
            failwith "Note 0 was admitted";
          if Effect_ctx.dispatch ctx (Note 1) <> Executed then
            failwith "Note 1 was refused"
      | Bad_reply_effect -> ignore (Effect_ctx.reply ctx ping_events)
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

(* ------------------------------------------------------------------------ *)
(* Counter: a leaf with no Effects, so no Service binding. *)

type counter_state = { n : int }
type inc = Inc

let counter_actions =
  Message.make ~name:"counter.action" ~kind:(fun Inc -> "counter.inc") ()

let counter : (counter_state, unit, inc, never, never) Component.t =
  Component.make ~name:"counter" ~actions:counter_actions ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx Inc _ -> Ctx.update ctx (fun s -> { n = s.n + 1 }))
    ()

(* ------------------------------------------------------------------------ *)
(* App *)

type app_state = { a : downloader_state; b : downloader_state; counter : counter_state }
type app_services = { downloads : download_service }
type noop = Noop

let a_slot = Child.make "a" downloader
let b_slot = Child.make "b" downloader
let counter_slot = Child.make "counter" counter
let app_actions = Message.make ~name:"app.action" ~kind:(fun Noop -> "app.noop") ()

let app : (app_state, app_services, noop, never, never) Component.t =
  Component.make ~name:"app" ~actions:app_actions ~effects:Message.never
    ~outputs:Message.never
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
  log : Observe.Trace_log.log;
  sink : Sink.t;
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
  freeze_exn reg

let fixture_from ?clock state downloads worker_capacity =
  let p = protocol () in
  let sink, inbox = Event_bus.create p ~worker_capacity in
  let store = Store.create_exn ?clock p ~state ~services:{ downloads } inbox in
  let log = Observe.Trace_log.create () in
  Store.add_observer store (Observe.Trace_log.observer log);
  let root = Path.root app in
  {
    store;
    log;
    sink;
    a = Protocol.resolve_exn p (Path.child root a_slot);
    b = Protocol.resolve_exn p (Path.child root b_slot);
    counter = Protocol.resolve_exn p (Path.child root counter_slot);
  }

let fixture_with ?clock downloads worker_capacity =
  fixture_from ?clock empty_app downloads worker_capacity

let fixture downloads worker_capacity = fixture_with downloads worker_capacity

let drain_until store done_ =
  let deadline = Unix.gettimeofday () +. 5.0 in
  while not (done_ (Store.state store)) do
    (match Store.drain_events store 64 with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "no fault");
    if Unix.gettimeofday () >= deadline then Alcotest.fail "timed out waiting for replies";
    Thread.delay 0.001
  done
