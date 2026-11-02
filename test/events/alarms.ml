(* Alarms on the timer Service: the component the Rust port's tests/timer.rs
   and tests/review_timer_edges.rs each define (the two are the same
   machine). A [Set] arms a timer pinned to a new request; its [fired] Event
   moves the tag from pending to fired. *)

open Causet

type alarm_state = { tags : (Request_id.t * string) list; fired : string list }

type alarm_action =
  | Set of { tag : string; after_ms : int }
  | Fired of { request : Request_id.t }

type alarm_effect = Arm of { request : Request_id.t; after_ms : int }

let alarm_actions : alarm_action Message.t =
  Message.make ~name:"alarms.action"
    ~kind:(function Set _ -> "alarms.set" | Fired _ -> "alarms.fired")
    ()

let alarm_effects : alarm_effect Message.t =
  Message.make ~name:"alarms.effect" ~kind:(fun (Arm _) -> "alarms.arm") ()

let ms = 1_000_000

let alarms : (alarm_state, Timer.t, alarm_action, alarm_effect, never) component =
  Component.make ~name:"alarms" ~actions:alarm_actions ~effects:alarm_effects
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | Set { tag; after_ms } ->
          let request = Ctx.new_request ctx in
          Ctx.update ctx (fun s -> { s with tags = s.tags @ [ (request, tag) ] });
          Ctx.emit_effect ctx (Arm { request; after_ms })
      | Fired { request } -> (
          let s = Ctx.state ctx in
          match List.assoc_opt request s.tags with
          | None -> failwith "an armed alarm"
          | Some tag ->
              Ctx.set_state ctx
                { tags = List.remove_assoc request s.tags; fired = s.fired @ [ tag ] }))
    ~handle:(fun ctx (Arm { request; after_ms }) _ ->
      let reply = Effect_ctx.reply ctx Timer.fired in
      Timer.after (Effect_ctx.service ctx) ~delay:(after_ms * ms) reply request)
    ~register:(fun r ->
      Registrar.event r Timer.fired (fun (fired : Timer.fired) port ->
          Port.emit_action port (Fired { request = fired.request })))
    ()

let empty = { tags = []; fired = [] }

let protocol () =
  let reg = Registry.create () in
  Registry.root reg alarms;
  match Registry.freeze reg with
  | Ok p -> p
  | Error problems -> failwith (String.concat "; " (List.map problem_message problems))

let mount p = Protocol.resolve_exn p (Path.root alarms)

let set store m tag after_ms =
  match Store.dispatch_to store m (Set { tag; after_ms }) with
  | Ok Executed -> ()
  | Ok Disabled -> Alcotest.fail "Set refused"
  | Ok Faulted -> Alcotest.fail "Set faulted"
  | Error (Faulted msg) -> Alcotest.failf "faulted: %s" msg
  | Error _ -> Alcotest.fail "dispatch failed"

let drain store n =
  match Store.drain_events store n with
  | Ok k -> k
  | Error (Faulted msg) -> Alcotest.failf "faulted: %s" msg
  | Error _ -> Alcotest.fail "drain failed"
