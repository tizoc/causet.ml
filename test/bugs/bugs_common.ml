(* Shared by the tests of this directory: printing helpers, and the downloads
   scenario of the Rust port's tests/common/mod.rs, a [downloader] with
   Effects mounted twice (/a, /b) sharing one download Service, and a
   [counter] leaf with no Effects. *)

open Causet

(* ------------------------------------------------------------------------ *)
(* Printing *)

let boundary_string = function
  | Observe.Start -> "Start"
  | Finish -> "Finish"
  | Settled -> "Settled"
  | Bus -> "Bus"
  | Refused -> "Refused"
  | Inline -> "Inline"

let phase_string = function
  | Observe.Admission -> "Admission"
  | Reducer -> "Reducer"
  | Handler -> "Handler"
  | Adapter -> "Adapter"
  | Clock -> "Clock"
  | Observer b -> "Observer(" ^ boundary_string b ^ ")"
  | Invariant n -> "Invariant(" ^ n ^ ")"
  | Invariant_panic n -> "InvariantPanic(" ^ n ^ ")"

let at_string = function None -> "None" | Some id -> string_of_int id

(* A report as the Rust tests' [summary]: message, phase, execution, path. *)
let summary (r : Observe.Bug_report.t) =
  match r.place with
  | None -> Alcotest.failf "%s: made outside a Store" r.message
  | Some p ->
      Printf.sprintf "%s | %s | %s | %s" r.message (phase_string p.phase) (at_string p.at)
        p.path

let category_string = function
  | Meta.Action -> "Action"
  | Meta.Effect -> "Effect"
  | Meta.Event -> "Event"

let outcome_string = function
  | Ok Executed -> "Ok Executed"
  | Ok Disabled -> "Ok Disabled"
  | Ok Faulted -> "Ok Faulted"
  | Error Foreign_mount -> "Error Foreign_mount"
  | Error Foreign_bus -> "Error Foreign_bus"
  | Error (Faulted m) -> "Error (Faulted " ^ m ^ ")"

let check_outcome what expected got =
  Alcotest.(check string) what (outcome_string expected) (outcome_string got)

let ends_with ~suffix s =
  let n = String.length s and k = String.length suffix in
  n >= k && String.sub s (n - k) k = suffix

(* ------------------------------------------------------------------------ *)
(* Messages *)

type download_event =
  | Done of { request : Request_id.t; bytes : int }
  | Pushed of { note : string }

let download_events =
  Message.make ~name:"download.event"
    ~kind:(function Done _ -> "download.done" | Pushed _ -> "download.pushed")
    ()

(* Adapted by the Downloader in Rust, but never registered. *)
type ping = Ping

let pings : ping Message.t = Message.single "ping"

type downloader_action =
  | Fetch of string
  | Completed of { request : Request_id.t; bytes : int }
  | Finished of { url : string; bytes : int }
  | Note of int
  | Record_push of string  (** Rust: [DownloaderAction::Pushed(String)]. *)

let downloader_actions =
  Message.make ~name:"downloader.action"
    ~kind:(function
      | Fetch _ -> "downloader.fetch"
      | Completed _ -> "downloader.completed"
      | Finished _ -> "downloader.finished"
      | Note _ -> "downloader.note"
      | Record_push _ -> "downloader.pushed")
    ()

type downloader_effect = Fetch_effect of { request : Request_id.t; url : string }

let downloader_effects =
  Message.make ~name:"downloader.effect"
    ~kind:(fun (Fetch_effect _) -> "downloader.effect.fetch")
    ()

(* ------------------------------------------------------------------------ *)
(* The Service *)

type download_service = { fetch : Request_id.t -> string -> download_event reply -> unit }

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
  pending : (Request_id.t * string) list;
  done_ : (string * int) list;
  stale : Request_id.t list;
  notes : int list;
  pushed : string list;
}

let empty_downloader = { pending = []; done_ = []; stale = []; notes = []; pushed = [] }

let downloader =
  Component.make ~name:"downloader" ~actions:downloader_actions
    ~effects:downloader_effects ~outputs:Message.never
    ~is_enabled:(fun _ action _ -> match action with Note 0 -> false | _ -> true)
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
      | Note n -> Ctx.update ctx (fun s -> { s with notes = s.notes @ [ n ] })
      | Record_push note ->
          Ctx.update ctx (fun s -> { s with pushed = s.pushed @ [ note ] }))
    ~handle:(fun ctx (Fetch_effect { request; url }) _ ->
      let reply = Effect_ctx.reply ctx download_events in
      (Effect_ctx.service ctx).fetch request url reply)
    ~register:(fun r ->
      Registrar.event r download_events (fun ev port ->
          match ev with
          | Done { request; bytes } ->
              Port.emit_action port (Completed { request; bytes })
          | Pushed { note } -> Port.emit_action port (Record_push note)))
    ()

(* ------------------------------------------------------------------------ *)
(* Counter: a leaf with no Effects, so no Service binding. *)

type counter_state = { n : int }
type inc = Inc

let incs : inc Message.t =
  Message.make ~name:"counter.action" ~kind:(fun Inc -> "counter.inc") ()

let counter =
  Component.make ~name:"counter" ~actions:incs ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx Inc _ -> Ctx.update ctx (fun s -> { n = s.n + 1 }))
    ()

(* ------------------------------------------------------------------------ *)
(* App *)

type app_state = { a : downloader_state; b : downloader_state; counter : counter_state }
type app_services = { downloads : download_service }
type noop = Noop

let noops : noop Message.t =
  Message.make ~name:"app.action" ~kind:(fun Noop -> "app.noop") ()

let empty_app = { a = empty_downloader; b = empty_downloader; counter = { n = 0 } }
let a_slot = Child.make "a" downloader
let b_slot = Child.make "b" downloader
let counter_slot = Child.make "counter" counter

let app =
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

let protocol () =
  let reg = Registry.create () in
  Registry.root reg app;
  match Registry.freeze reg with
  | Ok p -> p
  | Error problems -> failwith (String.concat "; " (List.map problem_message problems))

type downloader_mount =
  (downloader_state, download_service, downloader_action, downloader_effect, never) mount

type fixture = {
  store : (app_state, app_services) Store.t;
  sink : Sink.t;
  a : downloader_mount;
  b : downloader_mount;
  counter_mount : (counter_state, unit, inc, never, never) mount;
}

let fixture downloads worker_capacity =
  let p = protocol () in
  let sink, inbox = Event_bus.create p ~worker_capacity in
  let store = Store.create_exn p ~state:empty_app ~services:{ downloads } inbox in
  Store.add_observer store (Observe.Trace_log.observer (Observe.Trace_log.create ()));
  let root = Path.root app in
  {
    store;
    sink;
    a = Protocol.resolve_exn p (Path.child root a_slot);
    b = Protocol.resolve_exn p (Path.child root b_slot);
    counter_mount = Protocol.resolve_exn p (Path.child root counter_slot);
  }
