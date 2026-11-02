<picture>
  <source media="(prefers-color-scheme: dark)" srcset="https://raw.githubusercontent.com/tizoc/causet.ml/main/assets/logo-dark.svg">
  <img src="https://raw.githubusercontent.com/tizoc/causet.ml/main/assets/logo.svg" alt="" width="120" height="36">
</picture>

# Causet

Causet is a framework for building event-driven systems as explicit,
composable state machines, with automatic bug finding through deterministic
simulation.

You define the operations your system can perform and the rules it must
obey. Causet generates simulated test runs that vary inputs, event delivery,
time and modeled failures. It checks your invariants after every input,
shrinks failing runs and gives you the choices needed to reproduce them.

> **Unreleased.** Neither package has been released to the opam repository
> yet. Pin both packages to the same source commit as shown below.

The [changelog](https://github.com/tizoc/causet.ml/blob/main/CHANGELOG.md) tracks notable changes.

It addresses a common problem in asynchronous software: the behavior of a
system becomes scattered across callbacks, tasks, timers and shared state.
Understanding a transition requires reconstructing what ran, in which
order, and which request a delayed result belonged to. Testing those
interactions reliably becomes difficult.

Causet gives that behavior a clear structure:

- **Actions** request state transitions. Reducers decide how state
  changes, and no other code changes it.
- **Effects** request external work. Services perform networking, storage
  and other I/O.
- **Events** bring results back. Request identities and reply routes tie
  each result to the request and the component that made it, so a reducer
  can tell a stale result from the one it is waiting for.
- **Outputs** connect components. A parent decides how a child's facts
  affect the wider application.

```text
          In production                     In a test

   ┌─────────────────────────┐     ┌─────────────────────────┐
   │ network, disk, clock    │     │ simulated network, disk │
   │                         │     │ and a virtual clock     │
   └──────▲───────────┬──────┘     └──────▲───────────┬──────┘
          │ Effects   │ Events            │ Effects   │ Events
   ┌──────┴───────────▼──────┐     ┌──────┴───────────▼──────┐
   │ your components:        │     │ the same components;    │
   │ one input at a time,    │     │ the test decides what   │
   │ in a fixed order        │     │ arrives, and when       │
   └─────────────────────────┘     └─────────────────────────┘
```

The components never touch the world directly: they ask for work with
Effects and hear back through Events. So the same components run against
the real world or a simulated one. They handle one input at a time, in a
fixed order, with time from the Store's clock, so a test can repeat a run
exactly (Guarantees below says on what conditions).

Its main advantages are:

- **Automatic bug discovery.** Generated simulations explore operation
  sequences and event timing while checking application invariants.
- **Reproducible failures.** Shrinking reduces a failing run to a small
  sequence of choices that reproduces it. Individual transitions can also
  be tested from deliberately constructed states.
- **Predictable execution.** Explicit scheduling and admission checks make
  ordering and transition rules easier to reason about.
- **Reusable components.** Components own their state and protocols, while
  their parent owns integration and policy. They can live in independent
  libraries.
- **Causal observability.** Execution metadata connects actions, effects
  and component emissions, helping explain how the system reached a
  particular outcome.

The cost is more explicit protocol and lifecycle code. That investment is
especially useful when the hard parts are ordering, timeouts, retries,
cancellation and stale asynchronous results, as in network protocols,
wallets and synchronization engines.

## Getting started

Replace `COMMIT_SHA` with a full commit of this repository. Both pins use
the same `dev` version so the packages also work together with tests enabled.

<!-- $MDX skip -->
```sh
opam pin add --no-action causet-reporting.dev 'git+https://github.com/tizoc/causet.ml.git#COMMIT_SHA'
opam pin add --no-action causet.dev 'git+https://github.com/tizoc/causet.ml.git#COMMIT_SHA'
opam install causet causet-reporting
```

Then depend on `causet` in a dune file, and on `causet.test` in tests;
`causet.ppx` derives the message values:

```lisp
(library
 (name shop)
 (libraries causet)
 (preprocess (pps causet.ppx)))
```

In a clone of this repository, start with the shop, then run the examples:

<!-- $MDX skip -->
```sh
dune exec examples/shop/main.exe  # one checkout, then reordered replies in simulation
dune test test/events             # worker threads, timeouts and retries
dune test examples/p2p_node  # up to four nodes on a simulated network, in generated runs
```

The [examples index](examples/README.md) gives a reading order, entry points
and commands for the larger reference applications.

## Find bugs automatically

A simulated payment gateway has a total budget of 100. Its bug is forgetting
to deduct earlier approvals. This test generates charges and decides when
to deliver replies, checking that total payments stay within the budget.
It checks the fixed gateway, then finds and shrinks a failure in the faulty
one and verifies that replay fails the same way.

```ocaml
open Causet
open Causet_guide.Shop
open Causet_test

let spending rng ~forgets =
  let remaining = ref 100 in
  let gateway =
    {
      charge =
        (fun request amount reply ->
          let approved = amount <= !remaining in
          (* The planted bug: earlier approvals are forgotten. *)
          if approved && not forgets then remaining := !remaining - amount;
          assert (Reply.try_send reply { request; approved } = Ok ()));
    }
  in
  let node =
    Node.create ~name:"shop" payments ~state:{ pending = Pending.empty; paid = []; declined = [] }
      ~services:gateway ~session:1 (Clock.Manual.create 0)
  in
  let node =
    Node.check node "paid within budget" (fun s ->
        if List.fold_left ( + ) 0 s.paid <= 100 then Ok () else Error "overspent")
  in
  let mount = Node.mount node (Path.root payments) in
  while Rng.more rng do
    if Rng.chance rng 1 2 then
      ignore (Node.dispatch node mount (Charge { amount = Rng.between rng 1 100 }))
    else ignore (Node.deliver_next node)
  done;
  while Option.is_some (Node.deliver_next node) do
    ()
  done

let () =
  check (fun rng -> spending rng ~forgets:false); (* The fixed gateway passes. *)
  let found = Option.get (search default (fun rng -> spending rng ~forgets:true)) in
  assert (found.key = {|node shop: invariant "paid within budget"|});
  assert (List.length found.choices < found.original);
  Printf.printf "seed %d: %s\nshrunk %d choices to %d\nchoices [%s]\n" found.seed found.key
    found.original (List.length found.choices)
    (String.concat ", " (List.map string_of_int found.choices));
  (* Replay must reproduce the planted failure. *)
  try
    replay found.choices (fun rng -> spending rng ~forgets:true);
    assert false
  with Failed failure -> assert (failure.key = found.key)
```

```text
seed 1: node shop: invariant "paid within budget"
shrunk 13 choices to 7
choices [1, 1, 51, 1, 1, 48, 0]
```

The shrunk run charges 52 and 49, then delivers both replies: 101 paid from
a budget of 100. `search` lets this example assert that a known bug is found;
use `check` in ordinary tests to fail on a discovered violation. Keeping the
shrunk choices gives you a reproducible regression test.


Here is the payment component, with a gateway that applies a per-request
limit, from [the guide's shop](src/guide/shop.ml):

<!-- $MDX file=src/guide/shop.ml,part=payments -->
```ocaml
type payments_state = {
  pending : int Pending.t;
      (** Charges sent and not answered yet, by request: the first answer to each takes
          it, and any after it is stale. *)
  paid : int list;
  declined : int list;
}

type payments_action =
  | Charge of { amount : int }
  | Settled of { request : Request_id.t; approved : bool }
[@@deriving message ~name:"payments.action"]

(* Named apart from the Action it carries out; its kind is the Rust port's. *)
type payments_effect =
  | Charge_effect of { request : Request_id.t; amount : int } [@kind "payments.charge"]
[@@deriving message ~name:"payments.effect"]

(* The gateway's answer, published through the Event bus. *)
type charged = { request : Request_id.t; approved : bool }
[@@deriving message ~name:"payments.charged"]

(* The payments capability: anything that can charge and answer later. *)
type gateway = { charge : Request_id.t -> int -> charged reply -> unit }

(* Approves charges up to a limit, answering at once on the Store's thread.
   A real gateway would answer from a worker thread through the same reply. *)
let approve_up_to limit =
  {
    charge =
      (fun request amount reply ->
        match Reply.try_send reply { request; approved = amount <= limit } with
        | Ok () -> ()
        | Error _ -> failwith "the Store lane is never full");
  }

let payments =
  Component.make ~name:"payments" ~actions:payments_action_message
    ~effects:payments_effect_message ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | Charge { amount } ->
          (* Made during mutation, so it can be both remembered and sent. *)
          let request = Ctx.new_request ctx in
          Ctx.update ctx (fun s ->
              { s with pending = Pending.add request amount s.pending });
          Ctx.emit_effect ctx (Charge_effect { request; amount })
      | Settled { request; approved } -> (
          let s = Ctx.state ctx in
          (* An answer to no pending request is stale: ignored. *)
          match Pending.take request s.pending with
          | None -> ()
          | Some (amount, pending) ->
              Ctx.set_state ctx
                (if approved then { s with pending; paid = s.paid @ [ amount ] }
                 else { s with pending; declined = s.declined @ [ amount ] })))
    ~handle:(fun ctx (Charge_effect { request; amount }) _ ->
      let reply = Effect_ctx.reply ctx charged_message in
      (Effect_ctx.service ctx).charge request amount reply)
    ~register:(fun r ->
      Registrar.event r charged_message (fun (ev : charged) port ->
          Port.emit_action port (Settled { request = ev.request; approved = ev.approved })))
    ()
```

The guide's shop (`Causet_guide.Shop`) puts the same component under a
cart, with child components and Outputs.

Larger applications, each with transition tests and generated runs on a
simulated world (`dune test examples/<name>`):

- `examples/wallet_send`: a wallet send with paged input discovery,
  stale-result pins, retries and asynchronous proving;
- `examples/p2p` and `examples/p2p_node`: a reusable P2P component, generic
  over its messages, with several connections per peer, mounted in a node
  that fetches and serves over it; up to four nodes on a simulated network;
- `examples/chain_placement`: projections of a chain's stored reports into
  each block's placement, checked across restarts and against recomputing
  from scratch.

## Guarantees

- **Authority is typed.** Only reducers change State and start Effects;
  Effect handlers use Services and read State; Event adapters and Output
  lowerers emit Actions only.
- **Order is fixed.** Each input's consequences run depth first, in the
  order emitted, each Action admitted against the State of that moment.
- **Addresses are stable.** A mount's identity is a hash of its path,
  identical in every run; replies return to the mount that asked.
- **Runs repeat, on conditions.** From the same starting State, the same
  inputs at the same clock readings make the same run, provided the
  application's own code is deterministic (the guide's determinism page
  says what that takes). The one difference is each run's session
  identity, which request IDs carry.
- **Correlation, not policy.** Causet ties every answer to its request and
  returns it to the component that asked; whether an answer is still
  wanted is that component's reducer's decision.
- **Messages need no serialization.** They travel as typed values;
  serialization is the application's choice, for its own State.
- **Exceptions fault the Store** at the root input, with nothing resumed,
  even one an Effect handler catches from its `dispatch`. State set before
  the raise is kept: nothing is rolled back.
- **Bugs are reported, not fatal.** `Bug.condition` reports a state the
  code believes impossible and lets the caller's fallback run; reports
  carry their place and reach observers.

## Testing

`causet.test` runs the real Store:

- a `Node` holds every Event its Services send until the test delivers it,
  and checks the application's invariants after every input;
- `check` generates runs from a stream of choices, makes every passing run
  twice to check it is deterministic, and shrinks a failing run to a short
  list of choices that `replay` repeats;
- a `Scenario` runs several nodes on one virtual clock and a simulated
  world, and `causet.projection` checks projections across restarts and
  against recomputing from scratch.

```ocaml
module Rng = Causet_test.Rng

let every_order_is_accounted_for (s : shop_state) =
  let p = s.payments in
  let charges = List.length p.paid + List.length p.declined + Pending.cardinal p.pending in
  if charges = s.orders then Ok ()
  else Error (Printf.sprintf "%d orders, %d charges" s.orders charges)

let () =
  Causet_test.check (fun rng ->
      let shop =
        Node.of_protocol ~name:"shop" (protocol ()) ~state:empty
          ~services:{ gateway = approve_up_to 100 } ~session:1 (Clock.Manual.create 1_000)
      in
      let shop =
        Node.check shop "every order is paid, declined or pending" every_order_is_accounted_for
      in
      let cart = Node.mount shop (Path.child (Path.root shop_root) cart_slot) in
      while Rng.more rng do
        match Rng.below rng 3 with
        | 0 -> ignore (Node.deliver_next shop)
        | 1 -> ignore (Node.dispatch shop cart (Add { item = "tea"; price = Rng.between rng 1 60 }))
        | _ -> ignore (Node.dispatch shop cart Checkout)
      done)
```

The guide's [testing page](https://tizoc.github.io/causet/testing.html) has the rest: a simulated world shared by several
shops, and a planted bug found and shrunk.

## Documentation

[The guide](https://tizoc.github.io/causet/), shared with the Rust and TypeScript ports, explains causet one page
at a time, each example in all three languages:

- **overview**: the problem, the model, and one checkout's journey
  through the example shop; start here;
- **authority**: what each kind of code may read, change and start;
- **scheduling**: the order in which work runs, admission, and faults;
- **addressing**: mounts, paths, child slots, Outputs and replies, and
  where context and Effects belong;
- **determinism**: what application code must do so a run can be repeated;
- **testing**: one node, generated runs, several nodes on a simulated
  world, and the checks for projections.

Every example in the guide, and in this file, runs as a test: the guide's
OCaml examples are in `doc/*.md`. The API reference is `dune build @doc`,
then `_build/default/_doc/_html/causet/index.html`.

## Libraries

| Library | Contents |
| --- | --- |
| `causet` | Components, the Registry and Protocol, the Store and its scheduler, contexts, mounts, the Event bus, the clock, the timer Service, observers, bug reports and invariants. |
| `causet.ppx` | `[@@deriving message]`: a message type's name, kinds, log level and fields, and its printer; `[@@deriving debug]`, the printer alone. |
| `causet.observers` | Logging through `logs`, and execution statistics, as observers. |
| `causet.projection` | Projections of an ordered log of records into derived facts: warm-up after a restart, a check that restarts change nothing a processor outputs, and a check that incremental recomputation stores, for every key output, what recomputing from scratch would. |
| `causet.test` | Testing and simulation on the real Store: held Event delivery, simulated timers on a shared clock, invariants per node and over groups of nodes, seeded scenarios over several Stores, a simulated world's messages in flight, generated runs with a separate stream for the world and swarm steps, shrunk to small repeatable cases. |
| `causet.guide` | The guide's shop, which its examples run. |

Beside it, in this repository as a separate package:

| Package | Contents |
| --- | --- |
| `causet-reporting` | A reporting pipeline: deduplication, rate limits, sampling, metric aggregation, JSON-lines dumps, with the Rust crate's decisions bit for bit. It knows nothing of the Store; an application wires it in as a Service. |

## Development

<!-- $MDX skip -->
```sh
dune build                                 # everything, warnings as errors in dev
dune test                                  # unit, integration, example and documentation tests
dune build @doc                            # the API reference
dune exec --release bench/baseline.exe     # ns and words allocated per execution
dune fmt                                   # ocamlformat 0.27.0
```

OCaml 5.3 or later.
