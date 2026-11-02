(* Baseline benchmarks, the Rust port's benches/baseline.rs scenario for
   scenario. Run with [dune exec --release bench/baseline.exe]. Each
   scenario runs several rounds on a fresh Store and reports the median
   time per execution (every Action, Effect and Event the Store runs counts
   once), with the words allocated per execution in that round. A
   hand-written scheduler over one variant runs the queued-Action scenario
   as the reference point for the cost of the kernel's generality.

   [CAUSET_BENCH=<text>] runs only the scenarios whose name contains it (and
   the reference), and [CAUSET_BENCH_ROUNDS=<n>] sets the rounds. *)

open Causet

(* ------------------------------------------------------------------------ *)
(* Messages *)

type bench_action =
  | Fan of int  (** n queued Actions to self. *)
  | Noop of int  (** A small payload. *)
  | Refused_fan of int  (** n queued Actions the enabling check refuses. *)
  | Refused
  | Mixed of int  (** n pairs of a queued Effect and a queued Action. *)
  | Inline_children of int  (** n inline child calls. *)
  | Queued_children of int  (** n queued child sends. *)
  | Fanout of int
      (** n inline child calls, each reporting an Output lowered to 4 Actions. *)
  | Sync_children of int  (** One Effect dispatching n synchronous children. *)
  | Large_fan of int  (** n queued Actions carrying 4 KiB each. *)
  | Large of bytes
[@@deriving message ~name:"bench.action" ~no_pp]

type bench_effect = Noop_effect [@kind "bench.noop"] | Sync of int
[@@deriving message ~name:"bench.effect" ~no_pp]

type tick = { n : int } [@@deriving message ~name:"bench.tick" ~no_pp]
type kid_action = Ping | Emit [@@deriving message ~name:"kid.action" ~no_pp]
type done_ = Done [@kind "kid.done"] [@@deriving message ~name:"kid.done" ~no_pp]

(* ------------------------------------------------------------------------ *)
(* Components *)

type kid_state = { pings : int }

let kid =
  Component.make ~name:"kid" ~actions:kid_action_message ~effects:Message.never
    ~outputs:done_message
    ~reduce:(fun ctx action _ ->
      Ctx.update ctx (fun s -> { pings = s.pings + 1 });
      match action with Emit -> Ctx.emit_output ctx Done | Ping -> ())
    ()

type bench_state = { count : int; bytes : int; a : kid_state; b : kid_state }
type bench_services = { mutable effects : int }

let a_slot = Child.make "a" kid
let b_slot = Child.make "b" kid

let lower_done _ Done port =
  for i = 0 to 3 do
    Port.emit_action port (Noop i)
  done

let bench =
  Component.make ~name:"bench" ~actions:bench_action_message ~effects:bench_effect_message
    ~outputs:Message.never
    ~is_enabled:(fun _ action _ -> action <> Refused)
    ~reduce:(fun ctx action _ ->
      Ctx.update ctx (fun s -> { s with count = s.count + 1 });
      match action with
      | Fan n ->
          for i = 0 to n - 1 do
            Ctx.emit_action ctx (Noop i)
          done
      | Refused_fan n ->
          for _ = 1 to n do
            Ctx.emit_action ctx Refused
          done
      | Mixed n ->
          for i = 0 to n - 1 do
            Ctx.emit_effect ctx Noop_effect;
            Ctx.emit_action ctx (Noop i)
          done
      | Inline_children n ->
          for _ = 1 to n do
            ignore (Ctx.reduce_child ctx a_slot Ping)
          done
      | Queued_children n ->
          for _ = 1 to n do
            Ctx.to_child ctx a_slot Ping
          done
      | Fanout n ->
          for _ = 1 to n do
            ignore (Ctx.reduce_child ctx a_slot Emit)
          done
      | Sync_children n -> Ctx.emit_effect ctx (Sync n)
      | Large_fan n ->
          for _ = 1 to n do
            Ctx.emit_action ctx (Large (Bytes.make 4096 '\007'))
          done
      | Large payload ->
          Ctx.update ctx (fun s -> { s with bytes = s.bytes + Bytes.length payload })
      | Noop _ | Refused -> ())
    ~handle:(fun ctx effect_ _ ->
      let svc = Effect_ctx.service ctx in
      svc.effects <- svc.effects + 1;
      match effect_ with
      | Sync n ->
          for i = 0 to n - 1 do
            ignore (Effect_ctx.dispatch ctx (Noop i))
          done
      | Noop_effect -> ())
    ~register:(fun r ->
      Registrar.event r tick_message (fun t port -> Port.emit_action port (Noop t.n));
      Registrar.child r a_slot
        ~lens:(Lens.field ~get:(fun s -> s.a) ~set:(fun s a -> { s with a }))
        ~on_output:lower_done ();
      Registrar.child r b_slot
        ~lens:(Lens.field ~get:(fun s -> s.b) ~set:(fun s b -> { s with b }))
        ~on_output:lower_done ())
    ()

(* ------------------------------------------------------------------------ *)
(* Fixture *)

let protocol =
  let reg = Registry.create () in
  Registry.root reg bench;
  match Registry.freeze reg with Ok p -> p | Error _ -> failwith "invalid protocol"

let root_mount = Protocol.resolve_exn protocol (Path.root bench)
let a_mount = Protocol.resolve_exn protocol (Path.child (Path.root bench) a_slot)
let b_mount = Protocol.resolve_exn protocol (Path.child (Path.root bench) b_slot)

let fixture () =
  let sink, inbox = Event_bus.create protocol ~worker_capacity:(1 lsl 16) in
  let store =
    Store.create_exn protocol
      ~state:{ count = 0; bytes = 0; a = { pings = 0 }; b = { pings = 0 } }
      ~services:{ effects = 0 } inbox
  in
  (store, sink)

(* ------------------------------------------------------------------------ *)
(* Harness *)

let rounds =
  match Option.bind (Sys.getenv_opt "CAUSET_BENCH_ROUNDS") int_of_string_opt with
  | Some n -> n
  | None -> 7

let selected name =
  String.starts_with ~prefix:"reference" name
  ||
  match Sys.getenv_opt "CAUSET_BENCH" with
  | None -> true
  | Some only ->
      let n = String.length only and m = String.length name in
      let rec at i = i + n <= m && (String.sub name i n = only || at (i + 1)) in
      at 0

(* Root inputs per round; each fans out to [fan] items. *)
let roots = 1_000
let fan = 100

type sample = { elapsed : float; executions : int; words : float }
type row = { name : string; ns : float; words_per_exec : float; executions : int }

let now () = Mtime.Span.to_float_ns (Mtime_clock.elapsed ())

(* Times [run]; the words it allocated, minor and major, less promotions. *)
let timed run =
  Gc.full_major ();
  let w0 =
    Gc.minor_words () +. (Gc.quick_stat ()).major_words
    -. (Gc.quick_stat ()).promoted_words
  in
  let t0 = now () in
  let executions = run () in
  let elapsed = now () -. t0 in
  let s = Gc.quick_stat () in
  let w1 = s.minor_words +. s.major_words -. s.promoted_words in
  { elapsed; executions; words = w1 -. w0 }

(* Runs [round] several times: the median time per execution, and the
   words of the median round. *)
let measure name round =
  if not (selected name) then None
  else (
    ignore (round ());
    let samples =
      List.init rounds (fun _ -> round ())
      |> List.sort (fun a b ->
             Float.compare
               (a.elapsed /. float a.executions)
               (b.elapsed /. float b.executions))
    in
    let m = List.nth samples (rounds / 2) in
    Some
      {
        name;
        ns = m.elapsed /. float m.executions;
        words_per_exec = m.words /. float m.executions;
        executions = m.executions;
      })

(* Times [run] on a fresh Store; executions come from the Store's count,
   unless [counted] gives them. *)
let store_round ?observer ?counted run () =
  let store, sink = fixture () in
  Option.iter (fun o -> Store.add_observer store (o ())) observer;
  let s =
    timed (fun () ->
        run store sink;
        match counted with Some n -> n | None -> Store.executions store)
  in
  ignore (Sys.opaque_identity (Store.state store).count);
  s

let dispatch_roots store action =
  for _ = 1 to roots do
    match Store.dispatch_to store root_mount (action ()) with
    | Ok Executed -> ()
    | _ -> failwith "a root input was not executed"
  done

(* ------------------------------------------------------------------------ *)
(* The reference: one variant, one match, one FIFO per execution, the same
   depth-first order and the same causal metadata, no erasure. *)

module Reference = struct
  type act = Fan of int | Noop of int
  type t = { mutable last : int; mutable count : int; mutable executions : int }

  let rec execute t action causal =
    let prev = t.last in
    t.last <- t.last + 1;
    let id = t.last in
    let causal_id, ordinal = match causal with Some c -> c | None -> (id, 0) in
    ignore (Sys.opaque_identity (id, prev, causal_id, ordinal));
    t.executions <- t.executions + 1;
    t.count <- t.count + 1;
    let fifo = Queue.create () in
    (match action with
    | Fan n ->
        for i = 0 to n - 1 do
          Queue.push (Noop i) fifo
        done
    | Noop x -> ignore (Sys.opaque_identity x));
    let ordinal = ref 0 in
    Queue.iter
      (fun child ->
        execute t child (Some (id, !ordinal));
        incr ordinal)
      fifo
end

let observer_stats () = Causet_observers.Stats.observer (Causet_observers.Stats.create ())
let observer_logger () = Causet_observers.logger ()

let () =
  Logs.set_level None;
  let attempts = roots * (fan + 1) in
  let rows =
    List.filter_map Fun.id
      [
        measure "reference: one variant, queued Actions" (fun () ->
            let t = { Reference.last = 0; count = 0; executions = 0 } in
            timed (fun () ->
                for _ = 1 to roots do
                  Reference.execute t (Fan fan) None
                done;
                t.executions));
        measure "queued Actions (small payload)"
          (store_round (fun s _ -> dispatch_roots s (fun () -> Fan fan)));
        measure "queued Actions, Stats observer"
          (store_round ~observer:observer_stats (fun s _ ->
               dispatch_roots s (fun () -> Fan fan)));
        measure "queued Actions, Logger, logging off"
          (store_round ~observer:observer_logger (fun s _ ->
               dispatch_roots s (fun () -> Fan fan)));
        measure "queued Actions (4 KiB payload)"
          (store_round (fun s _ -> dispatch_roots s (fun () -> Large_fan fan)));
        (* Refusals and inline calls make no execution: each attempt counts. *)
        measure "refused Actions (per refusal)"
          (store_round ~counted:attempts (fun s _ ->
               dispatch_roots s (fun () -> Refused_fan fan)));
        measure "refused Actions, Stats observer"
          (store_round ~observer:observer_stats ~counted:attempts (fun s _ ->
               dispatch_roots s (fun () -> Refused_fan fan)));
        measure "refused Actions, Logger, logging off"
          (store_round ~observer:observer_logger ~counted:attempts (fun s _ ->
               dispatch_roots s (fun () -> Refused_fan fan)));
        measure "Effects and Actions, interleaved"
          (store_round (fun s _ -> dispatch_roots s (fun () -> Mixed (fan / 2))));
        measure "inline child calls (per call)"
          (store_round ~counted:attempts (fun s _ ->
               dispatch_roots s (fun () -> Inline_children fan)));
        measure "inline child calls, Logger, logging off"
          (store_round ~observer:observer_logger ~counted:attempts (fun s _ ->
               dispatch_roots s (fun () -> Inline_children fan)));
        measure "queued child sends"
          (store_round (fun s _ -> dispatch_roots s (fun () -> Queued_children fan)));
        measure "Output fan-out (1 Output -> 4 Actions)"
          (store_round (fun s _ -> dispatch_roots s (fun () -> Fanout (fan / 4))));
        measure "synchronous Effect children"
          (store_round (fun s _ -> dispatch_roots s (fun () -> Sync_children fan)));
        measure "two mounts, alternating root inputs"
          (store_round (fun s _ ->
               for i = 0 to (roots * fan) - 1 do
                 ignore
                   (Store.dispatch_exn s (if i mod 2 = 0 then a_mount else b_mount) Ping)
               done));
        measure "Event drain, Store lane"
          (store_round (fun s sink ->
               ignore (Store.drain_events s 0);
               for i = 0 to (roots * fan / 2) - 1 do
                 match Sink.try_send_to sink root_mount tick_message { n = i } with
                 | Ok () -> ()
                 | Error _ -> failwith "send"
               done;
               while Store.drain_events s 1024 <> Ok 0 do
                 ()
               done));
        measure "Event drain, worker lane"
          (store_round (fun s sink ->
               let n = roots * fan / 2 in
               Thread.join
                 (Thread.create
                    (fun () ->
                      for i = 0 to n - 1 do
                        match Sink.try_send_to sink root_mount tick_message { n = i } with
                        | Ok () -> ()
                        | Error _ -> failwith "send"
                      done)
                    ());
               while Store.drain_events s 1024 <> Ok 0 do
                 ()
               done));
      ]
  in
  let reference = (List.hd rows).ns in
  print_endline "| Scenario | ns / exec | x reference | words / exec | executions |";
  print_endline "| --- | ---: | ---: | ---: | ---: |";
  List.iter
    (fun r ->
      Printf.printf "| %s | %.1f | %.1f | %.1f | %d |\n" r.name r.ns (r.ns /. reference)
        r.words_per_exec r.executions)
    rows
