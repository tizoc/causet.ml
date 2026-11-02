(* What one report costs: building it, and building and accepting it; the
   Rust port's benches/accept.rs scenario for scenario. Run with
   [dune exec --profile release bench/reporting.exe]. Each scenario runs
   several rounds and reports the fastest round's time and minor-heap words
   per report.

   - [build]: an error report with a dedupe key, dropped at once.
   - [kept]: the same report accepted by a default pipeline with dedupe off;
     the rate limit's window is 0, so every report is kept.
   - [deduplicated]: the same report, repeated, accepted by a default
     pipeline: all but the first are dropped as repeats.
   - [aggregated]: a metric report whose policy samples none of them
     ([sample_rate] 0) and aggregates [took_ms] grouped by [ok], with the
     default window (30 s) and reservoir (128 samples); the rate limit's
     window is 0, so every report reaches sampling.

   Time advances 1 ms per report. [CAUSET_BENCH=<text>] runs only the
   scenarios whose name contains it, [CAUSET_BENCH_ROUNDS=<n>] sets the
   rounds and [CAUSET_BENCH_REPORTS=<n>] the reports per round. *)

open Causet_reporting
module P = Payload

let source = "p2p_rpc"

let error_report () =
  let payload, dedupe_key =
    P.(
      create "rpc_failed"
      |> field "peer" (String "12D3KooWQk")
      |> field "method" (String "get_block")
      |> field "code" (Uint 503) |> field "took_ms" (Uint 120)
      |> finish_with_dedupe_key "rpc_failed:get_block")
  in
  Report.make ~dedupe_key Error ~source payload

let metric_report i =
  P.(
    create "rpc"
    |> field "method" (String "get_block")
    |> field "took_ms" (Uint (80 + (i * 37 mod 400)))
    |> field "ok" (Bool (i mod 7 <> 0))
    |> finish)
  |> Report.make Metric ~source

let no_rate_limit = { Config.Rate_limit.default with window_ms = 0 }

let kept_config =
  {
    Config.default with
    dedupe = { Config.default.dedupe with enabled = false };
    rate_limit = no_rate_limit;
  }

let aggregated_config =
  {
    Config.default with
    rate_limit = no_rate_limit;
    metrics =
      [
        {
          source;
          event = "rpc";
          sample_rate = Some 0.;
          aggregate =
            Some
              {
                Config.Aggregate.default with
                window_ms = 30_000;
                fields = [ "took_ms" ];
                group_by = [ "ok" ];
              };
        };
      ];
  }

let env name default =
  match Option.bind (Sys.getenv_opt name) int_of_string_opt with
  | Some n -> n
  | None -> default

let rounds = env "CAUSET_BENCH_ROUNDS" 7
let reports = env "CAUSET_BENCH_REPORTS" 1_000_000
let filter = Option.value (Sys.getenv_opt "CAUSET_BENCH") ~default:""

let contains s sub =
  let n = String.length sub in
  let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
  n = 0 || go 0

(* The fastest of [rounds] rounds of [reports] calls of [step], as
   nanoseconds and minor words per call. *)
let measure step =
  let warmup = min reports 10_000 in
  for i = 0 to warmup - 1 do
    step i
  done;
  let best = ref (infinity, 0.) and next = ref warmup in
  for _ = 1 to rounds do
    let words = Gc.minor_words () in
    let start = Unix.gettimeofday () in
    for i = !next to !next + reports - 1 do
      step i
    done;
    let ns = (Unix.gettimeofday () -. start) *. 1e9 /. float_of_int reports in
    let words = (Gc.minor_words () -. words) /. float_of_int reports in
    next := !next + reports;
    if ns < fst !best then best := (ns, words)
  done;
  !best

let total (s : Report.Stats.t) =
  s.accepted + s.dropped_disabled + s.dropped_rate_limited + s.dropped_deduped
  + s.dropped_sampled + s.dropped_invalid

let run name scenario =
  if contains name filter then
    let ns, words = scenario () in
    Printf.printf "%-14s %8.1f ns/report %8.1f minor words/report\n%!" name ns words

let () =
  run "build" (fun () ->
      measure (fun _ -> ignore (Sys.opaque_identity (error_report ()))));
  run "kept" (fun () ->
      let p = Pipeline.create ~config:kept_config () in
      let r =
        measure (fun i ->
            ignore (Sys.opaque_identity (Pipeline.accept_report p i (error_report ()))))
      in
      let s = Pipeline.stats p in
      assert (s.accepted = total s);
      r);
  run "deduplicated" (fun () ->
      let p = Pipeline.create () in
      let r =
        measure (fun i ->
            ignore (Sys.opaque_identity (Pipeline.accept_report p i (error_report ()))))
      in
      let s = Pipeline.stats p in
      assert (s.accepted = 1 && s.dropped_deduped + 1 = total s);
      r);
  run "aggregated" (fun () ->
      let p = Pipeline.create ~config:aggregated_config () in
      let r =
        measure (fun i ->
            ignore (Sys.opaque_identity (Pipeline.accept_report p i (metric_report i))))
      in
      let s = Pipeline.stats p in
      assert (s.accepted + s.dropped_sampled = total s);
      r)
