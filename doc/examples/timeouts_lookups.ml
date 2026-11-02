(* The directory's answer. *)
type listing = Listing of { request : Request_id.t; number : int }
[@@deriving message ~name:"lookup.answer"]

(* Anything that can look a name up and answer later, from any thread. *)
type directory = { find : Request_id.t -> string -> listing reply -> unit }
type lookup_services = { directory : directory; timer : Timer.t }

type lookup_state = {
  lookups : string Pending.t;  (** Each lookup's name, until its deadline. *)
  armed : (Request_id.t * int) option;
      (** The one timer armed: its own request, and the deadline it is for. *)
  found : (string * int) list;
  expired : string list;
}

type lookup_action =
  | Look_up of { name : string; wait_ms : int }
  | Listed of { request : Request_id.t; number : int }
  | Sweep of { timer : Request_id.t }
[@@deriving message ~name:"lookup.action"]

type lookup_effect =
  | Query of { request : Request_id.t; name : string }
  | Arm_sweep of { timer : Request_id.t; delay : int }
      (** Arms the one timer, [delay] nanoseconds from now. *)
[@@deriving message ~name:"lookup.effect"]

let lookup_ms = 1_000_000

(* One timer, for the earliest deadline: armed when none is, or when a deadline
   comes before the one it is armed for. *)
let arm_earliest ctx now =
  let s = Ctx.state ctx in
  let arm_at at =
    let timer = Ctx.new_request ctx in
    Ctx.set_state ctx { s with armed = Some (timer, at) };
    Some (Arm_sweep { timer; delay = at - now })
  in
  match (Pending.next_deadline s.lookups, s.armed) with
  | Some at, None -> arm_at at
  | Some at, Some (_, armed_at) when at < armed_at -> arm_at at
  | _ -> None

let lookups :
    (lookup_state, lookup_services, lookup_action, lookup_effect, never) Component.t =
  Component.make ~name:"lookups" ~actions:lookup_action_message
    ~effects:lookup_effect_message ~outputs:Message.never
    ~reduce:(fun ctx action (meta : Meta.t) ->
      let now = meta.time in
      let s = Ctx.state ctx in
      let ask =
        match action with
        | Look_up { name; wait_ms } ->
            let request = Ctx.new_request ctx in
            let deadline = now + (wait_ms * lookup_ms) in
            Ctx.set_state ctx
              { s with lookups = Pending.add ~deadline request name s.lookups };
            Some (Query { request; name })
        | Listed { request; number } ->
            (* Stale once expired: the deadline came first. *)
            (match Pending.take request s.lookups with
            | Some (name, lookups) ->
                Ctx.set_state ctx { s with lookups; found = s.found @ [ (name, number) ] }
            | None -> ());
            None
        | Sweep { timer } ->
            (* A timer replaced by an earlier one is stale. *)
            (match s.armed with
            | Some (armed, _) when armed = timer ->
                let due, lookups = Pending.expire now s.lookups in
                Ctx.set_state ctx
                  { s with lookups; armed = None; expired = s.expired @ List.map snd due }
            | _ -> ());
            None
      in
      let arm = arm_earliest ctx now in
      Option.iter (Ctx.emit_effect ctx) ask;
      Option.iter (Ctx.emit_effect ctx) arm)
    ~handle:(fun ctx eff _ ->
      let services = Effect_ctx.service ctx in
      match eff with
      | Query { request; name } ->
          services.directory.find request name (Effect_ctx.reply ctx listing_message)
      | Arm_sweep { timer; delay } ->
          Timer.after services.timer ~delay (Effect_ctx.reply ctx Timer.fired) timer)
    ~register:(fun r ->
      Registrar.event r listing_message (fun (Listing { request; number }) port ->
          Port.emit_action port (Listed { request; number }));
      Registrar.event r Timer.fired (fun (fired : Timer.fired) port ->
          Port.emit_action port (Sweep { timer = fired.request })))
    ()
