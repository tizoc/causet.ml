The causet guide's OCaml examples for its timeouts page, run by mdx
(`dune test doc`). The guide's pages are in its own repository and show
each block here by the name above it.

<!-- snippet: host-driven-timer -->
```ocaml
open Causet

let () =
  let timer, control = Timer.driven (Clock.system ()) in
  (* Put [timer] in Services. After each root input, arm the host's wakeup for: *)
  let next = Timer.Control.next_deadline control in
  (* When the host wakes, publish due Events, then drain the Store as usual: *)
  ignore (Timer.Control.fire_due control);
  assert (next = None);
  Timer.stop timer
```

<!-- snippet: quotes -->
<!-- $MDX file=examples/timeouts_quotes.ml -->
```ocaml
(* How long to wait for an answer, in nanoseconds, and how many times to ask. *)
let quote_timeout = 100_000_000
let quote_attempts = 2

(* A supplier's answer. *)
type quoted = Quoted of { request : Request_id.t; price : int }
[@@deriving message ~name:"quote.answer"]

(* Anything that can quote a price and answer later, from any thread. *)
type supplier = { ask : Request_id.t -> string -> quoted reply -> unit }
type quote_services = { supplier : supplier; timer : Timer.t }

(* What an answer needs: the item asked about, and which attempt this is. *)
type quote_attempt = { item : string; attempt : int }

type quote_state = {
  waiting : quote_attempt Pending.t;
  prices : (string * int) list;
  gave_up : string list;
  stale : int;  (** Answers and timers for requests no longer pending. *)
}

type quote_action =
  | Ask_quote of { item : string }
  | Quote_answered of { request : Request_id.t; price : int }
  | Quote_timed_out of { request : Request_id.t }
[@@deriving message ~name:"quote.action"]

type quote_effect =
  | Request_quote of { request : Request_id.t; item : string }
      (** Sends the request to the supplier. *)
  | Arm_quote_timer of { request : Request_id.t }
      (** Arms a timer pinned to the same request. *)
[@@deriving message ~name:"quote.effect"]

(* Every attempt is a new request: an answer to an earlier one is stale. *)
let ask_again ctx item attempt =
  let request = Ctx.new_request ctx in
  Ctx.update ctx (fun s ->
      { s with waiting = Pending.add request { item; attempt } s.waiting });
  Ctx.emit_effect ctx (Request_quote { request; item });
  Ctx.emit_effect ctx (Arm_quote_timer { request })

let quotes : (quote_state, quote_services, quote_action, quote_effect, never) Component.t
    =
  Component.make ~name:"quotes" ~actions:quote_action_message
    ~effects:quote_effect_message ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      let s = Ctx.state ctx in
      match action with
      | Ask_quote { item } -> ask_again ctx item 1
      | Quote_answered { request; price } -> (
          match Pending.take request s.waiting with
          | Some ({ item; _ }, waiting) ->
              Ctx.set_state ctx { s with waiting; prices = s.prices @ [ (item, price) ] }
          (* Too late: it timed out, and was asked again. *)
          | None -> Ctx.set_state ctx { s with stale = s.stale + 1 })
      | Quote_timed_out { request } -> (
          match Pending.take request s.waiting with
          | Some ({ item; attempt }, waiting) when attempt < quote_attempts ->
              Ctx.set_state ctx { s with waiting };
              ask_again ctx item (attempt + 1)
          | Some ({ item; _ }, waiting) ->
              Ctx.set_state ctx { s with waiting; gave_up = s.gave_up @ [ item ] }
          (* Answered in time: the timer is stale. *)
          | None -> Ctx.set_state ctx { s with stale = s.stale + 1 }))
    ~handle:(fun ctx eff _ ->
      let services = Effect_ctx.service ctx in
      match eff with
      | Request_quote { request; item } ->
          services.supplier.ask request item (Effect_ctx.reply ctx quoted_message)
      | Arm_quote_timer { request } ->
          let reply = Effect_ctx.reply ctx Timer.fired in
          Timer.after services.timer ~delay:quote_timeout reply request)
    ~register:(fun r ->
      Registrar.event r quoted_message (fun (Quoted { request; price }) port ->
          Port.emit_action port (Quote_answered { request; price }));
      Registrar.event r Timer.fired (fun (fired : Timer.fired) port ->
          Port.emit_action port (Quote_timed_out { request = fired.request })))
    ()
```

<!-- snippet: quotes-timeout -->
```ocaml
open Causet
open Causet_test

let ms = 1_000_000

(* Answers at once. A test node holds every Event until the test delivers it,
   so the test decides when the answer arrives. *)
let at_once =
  {
    ask =
      (fun request _item reply ->
        match Reply.try_send_as_worker reply (Quoted { request; price = 7 }) with
        | Ok () -> ()
        | Error _ -> failwith "held");
  }

let () =
  let clock = Clock.Manual.create (1_000 * ms) in
  let timer, control = Timer.simulated clock in
  let state = { waiting = Pending.empty; prices = []; gave_up = []; stale = 0 } in
  let node =
    Node.create ~name:"quotes" quotes ~state ~services:{ supplier = at_once; timer }
      ~session:1 clock
  in
  let node = Node.with_timer node control in
  let m = Node.mount node (Path.root quotes) in
  ignore (Node.dispatch node m (Ask_quote { item = "tea" }));
  assert (Node.next_deadline node = Some (1_100 * ms));
  (* 100 ms pass before the answer is delivered: the timer fires first. *)
  Clock.Manual.advance clock (100 * ms);
  assert (Node.fire_timers node = 1);
  assert (Node.deliver_where node Timer.fired (fun _ -> true));
  (* It asked again, as a new request with a timer of its own. *)
  let attempts s = List.map (fun a -> a.attempt) (Pending.values s.waiting) in
  assert (attempts (Node.state node) = [ 2 ]);
  assert (Node.next_deadline node = Some (1_200 * ms));
  (* The first answer arrives late, and is stale. The second is taken. *)
  ignore (Node.deliver_next node);
  assert ((Node.state node).stale = 1);
  ignore (Node.deliver_next node);
  assert ((Node.state node).prices = [ ("tea", 7) ]);
  (* The second timer fires after its answer, and finds nothing. *)
  Clock.Manual.advance clock (100 * ms);
  assert (Node.fire_timers node = 1);
  ignore (Node.deliver_next node);
  assert ((Node.state node).stale = 2);
  assert (Pending.is_empty (Node.state node).waiting)
```

<!-- snippet: pending-stale -->
```ocaml
let () =
  (* Request IDs as a reducer's [Ctx.new_request] makes them. *)
  let request index = { Request_id.session = 1; execution = 1; index } in
  let stale r pending = Option.is_none (Pending.take r pending) in
  (* The first of the answer and the timer takes the request; the second is stale. *)
  let pending = Pending.add (request 0) "tea" Pending.empty in
  let pending =
    match Pending.take (request 0) pending with
    | Some ("tea", pending) -> pending
    | _ -> assert false
  in
  assert (stale (request 0) pending);
  (* Cancelled: its answer, when it comes, is stale. *)
  let pending = Pending.add (request 1) "coffee" pending |> Pending.cancel (request 1) in
  assert (stale (request 1) pending);
  (* Order 7 ended: so did every request made for it. *)
  let pending =
    pending
    |> Pending.add (request 2) "order 7"
    |> Pending.add (request 3) "order 8"
    |> Pending.retain (fun _ work -> work <> "order 7")
  in
  assert (stale (request 2) pending);
  (* A new search replaces the one in flight. *)
  let pending =
    pending
    |> Pending.add (request 4) "first search"
    |> Pending.clear
    |> Pending.add (request 5) "second search"
  in
  assert (stale (request 4) pending);
  assert (Pending.cardinal pending = 1)
```

<!-- snippet: lookups -->
<!-- $MDX file=examples/timeouts_lookups.ml -->
```ocaml
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
```

<!-- snippet: lookups-run -->
```ocaml
(* Answers at once; the test node holds the answers. *)
let at_once =
  {
    find =
      (fun request name reply ->
        let number = String.length name in
        match Reply.try_send_as_worker reply (Listing { request; number }) with
        | Ok () -> ()
        | Error _ -> failwith "held");
  }

let () =
  let clock = Clock.Manual.create (1_000 * ms) in
  let timer, control = Timer.simulated clock in
  let state = { lookups = Pending.empty; armed = None; found = []; expired = [] } in
  let node =
    Node.create ~name:"lookups" lookups ~state ~services:{ directory = at_once; timer }
      ~session:1 clock
  in
  let node = Node.with_timer node control in
  let m = Node.mount node (Path.root lookups) in
  ignore (Node.dispatch node m (Look_up { name = "ada"; wait_ms = 300 }));
  assert (Node.next_deadline node = Some (1_300 * ms));
  (* An earlier deadline: a new timer, and the first one is stale. *)
  ignore (Node.dispatch node m (Look_up { name = "bob"; wait_ms = 100 }));
  ignore (Node.dispatch node m (Look_up { name = "cy"; wait_ms = 200 }));
  assert (Node.next_deadline node = Some (1_100 * ms));
  (* Only cy's answer arrives in time. *)
  let answers = List.map (fun (e : held_event) -> e.held_id) (Node.held node) in
  Node.deliver node (List.nth answers 2);
  assert ((Node.state node).found = [ ("cy", 2) ]);
  (* At 1,100 ms bob expires, and the timer is armed again, for ada. *)
  Clock.Manual.advance clock (100 * ms);
  assert (Node.fire_timers node = 1);
  assert (Node.deliver_where node Timer.fired (fun _ -> true));
  assert ((Node.state node).expired = [ "bob" ]);
  assert (Node.next_deadline node = Some (1_300 * ms));
  (* At 1,300 ms two timers fire: the stale first one finds nothing. *)
  Clock.Manual.advance clock (200 * ms);
  assert (Node.fire_timers node = 2);
  while Node.deliver_where node Timer.fired (fun _ -> true) do () done;
  assert ((Node.state node).expired = [ "bob"; "ada" ]);
  (* The answers for ada and bob arrive late, and are stale. *)
  Node.deliver node (List.nth answers 0);
  Node.deliver node (List.nth answers 1);
  assert ((Node.state node).found = [ ("cy", 2) ]);
  assert (Pending.is_empty (Node.state node).lookups)
```
