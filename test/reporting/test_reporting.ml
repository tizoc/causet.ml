(* The reporting pipeline: the Rust port's tests (pipeline_tests.rs, the
   payload combinators of alloc_tests.rs, review_reporting_edges.rs and the
   context trail's), BLAKE3 against the reference, and a 3,000-report run
   checked record for record against the Rust port's transcript of it. The
   Rust allocation and size tests have nothing to check here: there is no
   pool. *)

open Causet_reporting
module P = Payload

let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let source = "test"
let report ?dedupe_key kind payload = Report.make ?dedupe_key kind ~source payload
let msg kind = report kind P.(empty () |> field "msg" (String "test") |> finish)

let metric event value =
  report Metric P.(create event |> field "value" (Uint value) |> finish)

let kept (o : Report.outcome) = o.record <> None

let config_enabled =
  {
    Config.default with
    dedupe =
      {
        enabled = true;
        window_ms = 0;
        max_entries = 1024;
        error = true;
        metric = true;
        event = true;
        healthcheck = true;
      };
    sampling = { error = 1.0; metric = 1.0; event = 1.0; healthcheck = 1.0 };
  }

let json_of t = Yojson.Safe.from_string (P.to_json t)
let member path j = List.fold_left (fun j k -> Yojson.Safe.Util.member k j) j path

(* ------------------------------------------------------------------------ *)
(* BLAKE3 *)

let blake3_matches_the_reference () =
  let lines = In_channel.with_open_text "blake3_vectors.txt" In_channel.input_lines in
  check_int "vectors" 29 (List.length lines);
  List.iter
    (fun line ->
      Scanf.sscanf line "B %d %s" (fun len hex ->
          let input = String.init len (fun i -> Char.chr (i mod 251)) in
          check_string (string_of_int len) hex (Blake3.to_hex (Blake3.hash input))))
    lines

(* The pipeline feeds its hasher in small pieces: every split, and integers
   at every offset of a block, give the reference's hash. *)
(* A range outside the string is refused before anything is read: the
   copy below 17 bytes reads without bounds checks. *)
let blake3_refuses_a_range_outside_the_string () =
  let refused off len =
    let h = Blake3.create () in
    match Blake3.add_substring h "a" off len with
    | () -> false
    | exception Invalid_argument _ -> true
  in
  List.iter
    (fun (off, len) ->
      Alcotest.(check bool)
        (Printf.sprintf "off %d, len %d" off len)
        true (refused off len))
    [ (2, 1); (1, 1); (-1, 1); (0, 2); (0, -1); (max_int, 1) ];
  let h = Blake3.create () in
  Blake3.add_substring h "abc" 1 1;
  Blake3.add_substring h "abc" 3 0;
  Alcotest.(check string)
    "a range inside is taken"
    (Blake3.to_hex (Blake3.hash "b"))
    (Blake3.to_hex (Blake3.digest h))

let blake3_pieces_hash_as_one () =
  let lines = In_channel.with_open_text "blake3_vectors.txt" In_channel.input_lines in
  let h = Blake3.create () in
  List.iter
    (fun line ->
      Scanf.sscanf line "B %d %s" (fun len hex ->
          let input = String.init len (fun i -> Char.chr (i mod 251)) in
          List.iter
            (fun step ->
              let off = ref 0 and k = ref 0 in
              while !off < len do
                let n = min (len - !off) (1 + (!k * step mod 23)) in
                if n = 1 then Blake3.add_char h input.[!off]
                else Blake3.add_substring h input !off n;
                off := !off + n;
                incr k
              done;
              check_string
                (Printf.sprintf "%d in pieces of %d" len step)
                hex
                (Blake3.to_hex (Blake3.digest h)))
            [ 1; 5; 7 ]))
    lines;
  for prefix = 0 to 140 do
    let n = (prefix * 7919) - 1_000_000 in
    let bytes = Bytes.create 8 in
    Bytes.set_int64_le bytes 0 (Int64.of_int n);
    let pad = String.make prefix 'p' in
    let expected = Blake3.hash (pad ^ Bytes.to_string bytes ^ pad) in
    Blake3.add_string h pad;
    Blake3.add_int_le h n;
    Blake3.add_string h pad;
    check_string
      (Printf.sprintf "int after %d bytes" prefix)
      (Blake3.to_hex expected)
      (Blake3.to_hex (Blake3.digest h));
    Blake3.add_string h pad;
    Blake3.add_float_bits_le h (float_of_int n);
    Bytes.set_int64_le bytes 0 (Int64.bits_of_float (float_of_int n));
    check_bool "u64 prefix" true
      (Blake3.digest_u64_prefix h
      = Blake3.u64_prefix (Blake3.hash (pad ^ Bytes.to_string bytes)))
  done

(* ------------------------------------------------------------------------ *)
(* pipeline_tests.rs *)

let dedupe_drops_duplicate_within_window () =
  let p =
    Pipeline.create
      ~config:
        {
          config_enabled with
          dedupe = { config_enabled.dedupe with window_ms = 10; max_entries = 16 };
        }
      ()
  in
  check_bool "first" true (kept (Pipeline.accept_report p 0 (msg Metric)));
  check_bool "repeat" false (kept (Pipeline.accept_report p 5 (msg Metric)));
  check_bool "after the window" true (kept (Pipeline.accept_report p 20 (msg Metric)))

let rate_limit_drops_after_max_per_window () =
  let p =
    Pipeline.create
      ~config:{ config_enabled with rate_limit = { window_ms = 100; max_per_window = 1 } }
      ()
  in
  check_bool "first" true (kept (Pipeline.accept_report p 0 (msg Metric)));
  check_bool "second" false (kept (Pipeline.accept_report p 10 (msg Metric)));
  check_bool "after the window" true (kept (Pipeline.accept_report p 200 (msg Metric)))

let sampling_respects_rate () =
  let base =
    {
      config_enabled with
      dedupe = { config_enabled.dedupe with error = false };
      rate_limit = { window_ms = 1_000; max_per_window = 10 };
    }
  in
  let p =
    Pipeline.create ~config:{ base with sampling = { base.sampling with error = 0.0 } } ()
  in
  check_bool "rate 0 drops" false (kept (Pipeline.accept_report p 0 (msg Error)));
  let p = Pipeline.create ~config:base () in
  check_bool "rate 1 keeps" true (kept (Pipeline.accept_report p 0 (msg Error)))

let latency_policy ?aggregate rate =
  {
    config_enabled with
    rate_limit = { window_ms = 1_000; max_per_window = 10 };
    sampling = { config_enabled.sampling with metric = 0.0 };
    metrics = [ { source; event = "latency"; sample_rate = Some rate; aggregate } ];
  }

let metric_policy_overrides_sampling () =
  let p = Pipeline.create ~config:(latency_policy 1.0) () in
  check_bool "kept" true (kept (Pipeline.accept_report p 0 (metric "latency" 10)))

let value_aggregate =
  {
    Config.Aggregate.window_ms = 10;
    fields = [ "value" ];
    group_by = [];
    max_groups = 64;
    samples = 4;
  }

let count_of path payload =
  match
    Option.bind
      (List.fold_left
         (fun o k -> Option.bind o (fun o -> P.find_section o k))
         (Some payload) path)
      (fun o -> P.find_field o "count")
  with
  | Some (P.Uint n) -> n
  | _ -> Alcotest.fail "no count"

let metric_aggregate_emits_summary () =
  let p = Pipeline.create ~config:(latency_policy ~aggregate:value_aggregate 0.0) () in
  let o = Pipeline.accept_report p 0 (metric "latency" 10) in
  check_bool "no record" false (kept o);
  check_bool "no aggregate" true (o.aggregate = None);
  let o = Pipeline.accept_report p 5 (metric "latency" 10) in
  check_bool "still none" true (o.aggregate = None);
  let o = Pipeline.accept_report p 12 (metric "latency" 10) in
  check_bool "no record" false (kept o);
  match o.aggregate with
  | None -> Alcotest.fail "expected an aggregate"
  | Some r ->
      let payload = r.report.payload in
      Alcotest.(check (option string))
        "event" (Some "metric_aggregate") (P.event_str payload);
      let m = Option.get (P.find_section payload "metric") in
      Alcotest.(check bool)
        "source" true
        (P.find_field m "source" = Some (P.String "test"));
      Alcotest.(check bool)
        "event" true
        (P.find_field m "event" = Some (P.String "latency"));
      check_int "count" 2 (count_of [ "fields"; "value" ] payload)

let metric_aggregate_flushes_on_timeout () =
  let p = Pipeline.create ~config:(latency_policy ~aggregate:value_aggregate 0.0) () in
  ignore (Pipeline.accept_report p 0 (metric "latency" 10));
  ignore (Pipeline.accept_report p 5 (metric "latency" 10));
  match Pipeline.flush_aggregates p 12 with
  | [ r ] ->
      check_int "count" 2 (count_of [ "fields"; "value" ] r.payload);
      check_int "nothing after" 0 (List.length (Pipeline.flush_aggregates p 25))
  | l -> Alcotest.failf "expected one aggregate, got %d" (List.length l)

let metric_aggregate_groups_by_reason () =
  let config =
    {
      (latency_policy 0.0) with
      metrics =
        [
          {
            source = "block_producer";
            event = "won_slot_discarded";
            sample_rate = Some 0.0;
            aggregate =
              Some
                {
                  value_aggregate with
                  fields = [];
                  group_by = [ "reason" ];
                  max_groups = 8;
                  samples = 0;
                };
          };
        ];
    }
  in
  let p = Pipeline.create ~config () in
  let r reason =
    Report.make Metric ~source:"block_producer"
      P.(create "won_slot_discarded" |> field "reason" (String reason) |> finish)
  in
  check_bool "a" true ((Pipeline.accept_report p 0 (r "BestTipSuperior")).aggregate = None);
  check_bool "b" true ((Pipeline.accept_report p 5 (r "BestTipSuperior")).aggregate = None);
  match (Pipeline.accept_report p 12 (r "Timeout")).aggregate with
  | None -> Alcotest.fail "expected an aggregate"
  | Some a ->
      let reason =
        Option.get
          (Option.bind (P.find_section a.report.payload "groups") (fun g ->
               P.find_section g "reason"))
      in
      check_bool "counted" true (P.find_field reason "BestTipSuperior" = Some (P.Uint 2));
      check_bool "next window" true (P.find_field reason "Timeout" = None)

(* The aggregate's percentiles are nearest-rank: the ceil(p * n)-th smallest
   sample. Of 120, 80, 95 and 300 that is 95 for p50 and 300 for p95; the
   lower sample (120) was reported for p95 before. *)
let metric_aggregate_percentiles_are_nearest_rank () =
  let config =
    {
      config_enabled with
      metrics =
        [
          {
            source = "rpc";
            event = "call";
            sample_rate = Some 0.0;
            aggregate =
              Some
                {
                  window_ms = 10;
                  fields = [ "took_ms" ];
                  group_by = [ "ok" ];
                  max_groups = 8;
                  samples = 128;
                };
          };
        ];
    }
  in
  let p = Pipeline.create ~config () in
  List.iteri
    (fun i took_ms ->
      let r =
        Report.make Metric ~source:"rpc"
          P.(
            create "call"
            |> field "took_ms" (Uint took_ms)
            |> field "ok" (Bool (i <> 1))
            |> finish)
      in
      let o = Pipeline.accept_report p i r in
      check_bool "dropped, no aggregate" true (o.record = None && o.aggregate = None))
    [ 120; 80; 95; 300 ];
  match Pipeline.flush_aggregates p 10 with
  | [ r ] ->
      let section path =
        List.fold_left (fun o k -> Option.get (P.find_section o k)) r.payload path
      in
      let took_ms = section [ "fields"; "took_ms" ] in
      let number key =
        match P.find_field took_ms key with
        | Some (P.Float x) -> x
        | _ -> Alcotest.failf "no %s" key
      in
      Alcotest.(check (float 0.)) "min" 80. (number "min");
      Alcotest.(check (float 0.)) "max" 300. (number "max");
      Alcotest.(check (float 0.)) "p50" 95. (number "p50");
      Alcotest.(check (float 0.)) "p95" 300. (number "p95");
      let ok = section [ "groups"; "ok" ] in
      check_bool "true" true (P.find_field ok "true" = Some (P.Uint 3));
      check_bool "false" true (P.find_field ok "false" = Some (P.Uint 1))
  | l -> Alcotest.failf "expected one aggregate, got %d" (List.length l)

(* A NaN sample sorts after every number, as in the other ports: of 120,
   NaN, 80 and 300, p50 is 120 and p95 the NaN. Min and max skip it. *)
let metric_aggregate_sorts_nan_last () =
  let config =
    {
      config_enabled with
      dedupe = { config_enabled.dedupe with enabled = false };
      metrics =
        [
          {
            source = "rpc";
            event = "call";
            sample_rate = Some 0.0;
            aggregate =
              Some
                {
                  window_ms = 10;
                  fields = [ "took_ms" ];
                  group_by = [];
                  max_groups = 8;
                  samples = 128;
                };
          };
        ];
    }
  in
  let p = Pipeline.create ~config () in
  List.iteri
    (fun i took_ms ->
      let r =
        Report.make Metric ~source:"rpc"
          P.(create "call" |> field "took_ms" (Float took_ms) |> finish)
      in
      ignore (Pipeline.accept_report p i r))
    [ 120.; Float.nan; 80.; 300. ];
  match Pipeline.flush_aggregates p 10 with
  | [ r ] ->
      let took_ms =
        List.fold_left
          (fun o k -> Option.get (P.find_section o k))
          r.payload [ "fields"; "took_ms" ]
      in
      let number key =
        match P.find_field took_ms key with
        | Some (P.Float x) -> x
        | _ -> Alcotest.failf "no %s" key
      in
      Alcotest.(check (float 0.)) "min" 80. (number "min");
      Alcotest.(check (float 0.)) "max" 300. (number "max");
      Alcotest.(check (float 0.)) "p50" 120. (number "p50");
      check_bool "p95 is the NaN" true (Float.is_nan (number "p95"))
  | l -> Alcotest.failf "expected one aggregate, got %d" (List.length l)

let temp_dir prefix =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "causet-reporting-%s-%d-%f" prefix (Unix.getpid ())
         (Unix.gettimeofday ()))
  in
  Unix.mkdir dir 0o755;
  dir

let record ?(id = 1) timestamp_ms value =
  {
    Report.Record.id;
    timestamp_ms;
    report = report Metric P.(empty () |> field "value" (Uint value) |> finish);
    run_id = None;
  }

let filesystem_dump_writes_jsonl () =
  let dir = temp_dir "reports" in
  let fs = Filesystem.create { Config.Dump.default with dir; max_file_size_bytes = 0 } in
  Filesystem.dump fs (record 123 1);
  let contents =
    In_channel.with_open_bin (Filename.concat dir "reports.jsonl") In_channel.input_all
  in
  check_bool "newline" true (String.ends_with ~suffix:"\n" contents);
  let j = Yojson.Safe.from_string (String.trim contents) in
  check_bool "source" true (member [ "source" ] j = `String "test");
  check_bool "value" true (member [ "payload"; "value" ] j = `Int 1)

let filesystem_dump_rotates_on_size () =
  let dir = temp_dir "reports-rotate" in
  let fs = Filesystem.create { Config.Dump.default with dir; max_file_size_bytes = 1 } in
  Filesystem.dump fs (record 100 1);
  Filesystem.dump fs (record ~id:2 200 2);
  let files = List.sort compare (Array.to_list (Sys.readdir dir)) in
  Alcotest.(check (list string)) "files" [ "reports-200.jsonl"; "reports.jsonl" ] files

(* ------------------------------------------------------------------------ *)
(* Payload combinators (alloc_tests.rs) *)

let section_combinators_build_expected_payload () =
  let j =
    json_of
      P.(
        create "metric"
        |> section_fields "counts" [ ("ok", Uint 3); ("err", Uint 1) ]
        |> section_objects "stats"
             [ ("latency", (10, 42)); ("proofs", (1, 2)) ]
             (fun o (lo, hi) -> o |> field "min" (Uint lo) |> field "max" (Uint hi))
        |> finish)
  in
  check_string "json"
    {|{"event":"metric","counts":{"ok":3,"err":1},"stats":{"latency":{"min":10,"max":42},"proofs":{"min":1,"max":2}}}|}
    (Yojson.Safe.to_string j)

let array_combinators_build_expected_payload () =
  let p =
    P.(
      create "metric"
      |> array_values "counts" [ Uint 1; Uint 2; Uint 3 ]
      |> array_objects "peers"
           [ ("peer-a", Some 12); ("peer-b", None) ]
           (fun o (peer, rtt) ->
             o |> field "peer_id" (String peer)
             |> field_opt "rtt_ms" (Option.map (fun n -> Uint n) rtt))
      |> finish)
  in
  check_string "json"
    {|{"event":"metric","counts":[1,2,3],"peers":[{"peer_id":"peer-a","rtt_ms":12},{"peer_id":"peer-b"}]}|}
    (P.to_json p)

let optional_fields_include_only_present_values () =
  let p =
    P.(
      create "metric"
      |> field_opt "present" (Some (Uint 7))
      |> field_opt "missing" None |> finish)
  in
  check_string "json" {|{"event":"metric","present":7}|} (P.to_json p)

(* Fields, then objects, then arrays, whatever order they were added in. *)
let fields_come_before_objects_and_arrays () =
  let p =
    P.(
      empty ()
      |> array_values "a" [ Bool true ]
      |> section "s" (field "x" Null)
      |> field "f" (Int (-2)) |> finish)
  in
  check_string "json" {|{"f":-2,"s":{"x":null},"a":[true]}|} (P.to_json p)

let finish_with_dedupe_key_attaches_the_key () =
  let _, key =
    P.(create "metric" |> field "value" (Uint 7) |> finish_with_dedupe_key "metric:7")
  in
  check_string "key" "metric:7" key

(* Rust's [Display] of an [f64], which the dump writes. *)
let floats_print_as_rust_displays_them () =
  Alcotest.(check (list string))
    "floats"
    [
      "0.1";
      "2";
      "-3.5";
      "1000000000000000000000";
      "0.0000001";
      "123.456";
      "0";
      "-0";
      "0.3333333333333333";
      "inf";
      "NaN";
      "1000000000000000";
    ]
    (List.map P.float_to_string
       [ 0.1; 2.0; -3.5; 1e21; 1e-7; 123.456; 0.0; -0.0; 1.0 /. 3.0; infinity; nan; 1e15 ]);
  check_string "not finite" {|{"x":null}|}
    (P.to_json P.(empty () |> field "x" (Float infinity) |> finish))

(* The rate is used at [f32] precision, as in the Rust port: these are the
   bounds Rust computes. At 0.7, a double would move the bound by about
   one hash in 10^8, which no generated run would notice. *)
let sampling_bounds_are_rusts () =
  Alcotest.(check (list string))
    "bounds"
    [
      "12912720631694360576";
      "5534023442015191040";
      "1844674434858745856";
      "16602069226533945344";
      "2277375828536852480";
      "1844674432";
      "18446725382011879424";
      "9223372036854775808";
    ]
    (List.map
       (fun r -> Printf.sprintf "%Lu" (Pipeline.threshold r))
       [ 0.7; 0.3; 0.1; 0.9; 0.123456789; 1e-10; 0.999999; 0.5 ])

(* ------------------------------------------------------------------------ *)
(* review_reporting_edges.rs *)

let error payload = report Error payload

let nested_and_sibling_objects_hash_apart () =
  let siblings =
    P.(
      create "shape"
      |> section "x" (field "value" (Uint 1))
      |> section "y" (field "value" (Uint 2))
      |> finish)
  in
  let nested =
    P.(
      create "shape"
      |> section "x" (fun x ->
             x |> field "value" (Uint 1) |> section "y" (field "value" (Uint 2)))
      |> finish)
  in
  check_bool "json differs" true (P.to_json siblings <> P.to_json nested);
  let p = Pipeline.create () in
  check_bool "first" true (kept (Pipeline.accept_report p 100 (error siblings)));
  check_bool "distinct content kept" true
    (kept (Pipeline.accept_report p 101 (error nested)))

let repeats_in_one_millisecond_keep_the_dedupe_entry () =
  let p =
    Pipeline.create
      ~config:
        { Config.default with dedupe = { Config.Dedupe.default with max_entries = 2 } }
      ()
  in
  let accepted =
    List.init 4 (fun _ ->
        kept (Pipeline.accept_report p 100 (error P.(create "same" |> finish))))
  in
  Alcotest.(check (list bool)) "accepted" [ true; false; false; false ] accepted

let shared_dump_keeps_each_line_whole () =
  let dir = temp_dir "concurrent" in
  let fs = Filesystem.create { Config.Dump.default with dir; max_file_size_bytes = 0 } in
  let body = String.make 1024 'x' in
  let writers = 8 and each = 4 in
  let threads =
    List.init writers (fun w ->
        Thread.create
          (fun () ->
            for i = 0 to each - 1 do
              Filesystem.dump fs
                {
                  Report.Record.id = (w * each) + i + 1;
                  timestamp_ms = 100;
                  report =
                    error P.(create "concurrent" |> field "body" (String body) |> finish);
                  run_id = None;
                }
            done)
          ())
  in
  List.iter Thread.join threads;
  let lines =
    In_channel.with_open_bin (Filename.concat dir "reports.jsonl") In_channel.input_lines
  in
  check_int "lines" (writers * each) (List.length lines);
  List.iter (fun l -> ignore (Yojson.Safe.from_string l)) lines

let a_source_containing_the_separator_does_not_collide () =
  let r src key =
    Report.make Error ~source:src ~dedupe_key:key P.(create "e" |> finish)
  in
  let p = Pipeline.create () in
  check_bool "first" true (kept (Pipeline.accept_report p 100 (r "a" "x|e|y")));
  check_bool "different source and key" true
    (kept (Pipeline.accept_report p 101 (r "a|e|x" "y")))

(* ------------------------------------------------------------------------ *)
(* Configuration *)

let an_empty_object_is_the_default_configuration () =
  match Config.of_string "{}" with
  | Error e -> Alcotest.fail e
  | Ok c ->
      check_bool "enabled" true c.enabled;
      check_int "rate" 300 c.rate_limit.max_per_window;
      check_bool "dedupe on" true c.dedupe.enabled;
      check_string "file" "reports.jsonl" c.dump.filename

let a_json_configuration_reads_its_sections () =
  match
    Config.of_string
      {|{"sampling":{"metric":0.5},"dedupe":{"window_ms":7},
         "metrics":{"policies":[{"source":"net","event":"rtt","aggregate":{"fields":["ms"]}}]},
         "unknown":1}|}
  with
  | Error e -> Alcotest.fail e
  | Ok c ->
      Alcotest.(check (float 0.)) "metric" 0.5 c.sampling.metric;
      Alcotest.(check (float 0.)) "error" 1.0 c.sampling.error;
      (* As in the Rust port: a dedupe section without [enabled] is off. *)
      check_bool "dedupe off" false c.dedupe.enabled;
      check_int "window" 7 c.dedupe.window_ms;
      (match c.metrics with
      | [ { source = "net"; event = "rtt"; sample_rate = None; aggregate = Some a } ] ->
          Alcotest.(check (list string)) "fields" [ "ms" ] a.fields;
          check_int "samples" 128 a.samples
      | _ -> Alcotest.fail "policies");
      check_bool "a bad value is refused" true
        (Result.is_error (Config.of_string {|{"rate_limit":{"window_ms":-1}}|}))

(* ------------------------------------------------------------------------ *)
(* The context trail *)

let trail_overwrites_oldest () =
  let t = Context_trail.create 3 in
  List.iter (Context_trail.push t) [ 1; 2; 3 ];
  Alcotest.(check (list int)) "full" [ 1; 2; 3 ] (Context_trail.to_list t);
  Context_trail.push t 4;
  Alcotest.(check (list int)) "overwritten" [ 2; 3; 4 ] (Context_trail.to_list t)

let trail_recent_iterates_oldest_to_newest () =
  let t = Context_trail.create 4 in
  List.iter (Context_trail.push t) [ 10; 11; 12; 13; 14 ];
  Alcotest.(check (list int)) "recent" [ 13; 14 ] (Context_trail.recent t 2)

let trail_clear_resets () =
  let t = Context_trail.create 2 in
  List.iter (Context_trail.push t) [ 1; 2 ];
  Context_trail.clear t;
  check_bool "empty" true (Context_trail.is_empty t);
  Alcotest.(check (list int)) "nothing" [] (Context_trail.to_list t)

let zero_capacity_is_safe () =
  let t = Context_trail.create 0 in
  Context_trail.push t 1;
  check_bool "empty" true (Context_trail.is_empty t);
  check_int "capacity" 0 (Context_trail.capacity t);
  check_bool "no slot" true
    (Context_trail.get_or_push t (fun _ -> true) (fun () -> 1) = None)

(* ------------------------------------------------------------------------ *)
(* The Rust port's transcript: the same 3,000 reports, made by the same
   generator, give the same records, aggregates and counts, line for line;
   both ports flush aggregates in source and event order. The transcript is
   written by the Rust port's tests/pipeline_transcript.rs
   (CAUSET_WRITE_TRANSCRIPT=1) and copied here. *)

module Lcg = struct
  let state = ref 7L

  let next () =
    state := Int64.add (Int64.mul !state 6364136223846793005L) 1442695040888963407L;
    Int64.to_int (Int64.shift_right_logical !state 33)

  let below n = next () mod n
end

let sources = [| "net"; "disk"; "pipeline-ish" |]
let peers = [| "a"; "b"; "c"; "d"; "e\"q\\\n" |]
let floats = [| 0.1; 2.0; -3.5; 1e21; 1e-7; 123.456; 0.0; -0.0; 1.0 /. 3.0; infinity |]

let generated () =
  let open Lcg in
  let kind : Report.kind =
    match below 4 with 0 -> Error | 1 -> Metric | 2 -> Event | _ -> Healthcheck
  in
  let source = sources.(below 3) in
  let event =
    match below 4 with 0 -> "latency" | 1 -> "drop" | 2 -> "boot" | _ -> "tick"
  in
  let b = ref (P.create event) in
  let add f = b := f !b in
  if below 2 = 0 then add (P.field "value" (Uint (below 20)));
  if below 3 = 0 then add (P.field "size" (Int (-below 5)));
  if below 3 = 0 then add (P.field "ratio" (Float floats.(below 10)));
  if below 2 = 0 then add (P.field "peer" (String peers.(below 5)));
  if below 3 = 0 then add (P.field "ok" (Bool (below 2 = 0)));
  if below 4 = 0 then add (P.field "reason" (String [| "full"; "slow" |].(below 2)));
  (if below 4 = 0 then
     let n = below 3 in
     add
       (P.section "detail" (fun s ->
            let s = P.field "n" (Uint n) s in
            if n = 2 then P.section "inner" (P.field "deep" (Bool true)) s else s)));
  (if below 5 = 0 then
     let k = below 4 in
     add (P.array_values "list" (List.init k (fun i -> P.Uint (i * 3)))));
  (if below 6 = 0 then
     let k = below 3 in
     add
       (P.array_objects "items" (List.init k Fun.id) (fun o i -> P.field "i" (Uint i) o)));
  let payload, dedupe_key =
    if below 5 = 0 then
      let p, k = P.finish_with_dedupe_key [| "k1"; "k2" |].(below 2) !b in
      (p, Some k)
    else (P.finish !b, None)
  in
  Report.make ?dedupe_key kind ~source payload

let line tag = function
  | None -> tag ^ " -"
  | Some (r : Report.Record.t) ->
      tag ^ " " ^ Report.Record.to_json { r with run_id = None }

let transcript_config =
  {
    Config.default with
    dedupe =
      {
        enabled = true;
        window_ms = 50;
        max_entries = 8;
        error = true;
        metric = true;
        event = false;
        healthcheck = true;
      };
    rate_limit = { window_ms = 100; max_per_window = 12 };
    sampling = { error = 0.7; metric = 0.3; event = 0.5; healthcheck = 1.0 };
    metrics =
      [
        {
          source = "net";
          event = "latency";
          sample_rate = Some 0.25;
          aggregate =
            Some
              {
                window_ms = 200;
                fields = [ "value"; "size"; "ratio" ];
                group_by = [ "peer"; "ok" ];
                max_groups = 3;
                samples = 4;
              };
        };
        {
          source = "disk";
          event = "drop";
          sample_rate = None;
          aggregate =
            Some
              {
                window_ms = 100;
                fields = [];
                group_by = [ "reason"; "detail" ];
                max_groups = 8;
                samples = 0;
              };
        };
        { source = "disk"; event = "tick"; sample_rate = Some 0.9; aggregate = None };
      ];
  }

let the_rust_transcript_is_reproduced () =
  Lcg.state := 7L;
  let p = Pipeline.create ~config:transcript_config () in
  let out = ref [] and now = ref 0 in
  let emit l = out := l :: !out in
  for step = 0 to 2999 do
    now := !now + Lcg.below 8;
    let o = Pipeline.accept_report p !now (generated ()) in
    emit (line "R" o.record);
    if o.aggregate <> None then emit (line "A" o.aggregate);
    if step mod 97 = 0 then
      List.iter
        (fun r -> emit (line "F" (Pipeline.accept_aggregate_report p !now r)))
        (Pipeline.flush_aggregates p !now)
  done;
  let s = Pipeline.stats p in
  emit
    (Printf.sprintf "S %d %d %d %d %d %d" s.accepted s.dropped_disabled
       s.dropped_rate_limited s.dropped_deduped s.dropped_sampled s.dropped_invalid);
  let ours = List.rev !out in
  let rust = In_channel.with_open_text "pipeline_transcript.txt" In_channel.input_lines in
  check_int "lines" (List.length rust) (List.length ours);
  List.iteri
    (fun i (r, o) -> check_string (Printf.sprintf "line %d" (i + 1)) r o)
    (List.combine rust ours)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "reporting"
    [
      ( "blake3",
        [
          t "matches the reference" blake3_matches_the_reference;
          t "pieces hash as one" blake3_pieces_hash_as_one;
          t "refuses a range outside the string" blake3_refuses_a_range_outside_the_string;
        ] );
      ( "pipeline",
        [
          t "dedupe drops a duplicate within the window"
            dedupe_drops_duplicate_within_window;
          t "the rate limit drops after max per window"
            rate_limit_drops_after_max_per_window;
          t "sampling respects the rate" sampling_respects_rate;
          t "a metric policy overrides sampling" metric_policy_overrides_sampling;
          t "a metric aggregate emits a summary" metric_aggregate_emits_summary;
          t "a metric aggregate flushes on timeout" metric_aggregate_flushes_on_timeout;
          t "a metric aggregate groups by reason" metric_aggregate_groups_by_reason;
          t "a metric aggregate's percentiles are nearest-rank"
            metric_aggregate_percentiles_are_nearest_rank;
          t "a metric aggregate sorts a NaN last" metric_aggregate_sorts_nan_last;
          t "sampling bounds are Rust's" sampling_bounds_are_rusts;
          t "the dump writes JSON lines" filesystem_dump_writes_jsonl;
          t "the dump rotates on size" filesystem_dump_rotates_on_size;
        ] );
      ( "payload",
        [
          t "section combinators" section_combinators_build_expected_payload;
          t "array combinators" array_combinators_build_expected_payload;
          t "optional fields" optional_fields_include_only_present_values;
          t "fields come before objects and arrays" fields_come_before_objects_and_arrays;
          t "finish with a dedupe key" finish_with_dedupe_key_attaches_the_key;
          t "floats print as Rust displays them" floats_print_as_rust_displays_them;
        ] );
      ( "edges",
        [
          t "nested and sibling objects hash apart" nested_and_sibling_objects_hash_apart;
          t "repeats in one millisecond keep the dedupe entry"
            repeats_in_one_millisecond_keep_the_dedupe_entry;
          t "a shared dump keeps each line whole" shared_dump_keeps_each_line_whole;
          t "a source containing the separator does not collide"
            a_source_containing_the_separator_does_not_collide;
        ] );
      ( "config",
        [
          t "an empty object is the default" an_empty_object_is_the_default_configuration;
          t "a JSON configuration reads its sections"
            a_json_configuration_reads_its_sections;
        ] );
      ( "context trail",
        [
          t "overwrites the oldest" trail_overwrites_oldest;
          t "recent iterates oldest to newest" trail_recent_iterates_oldest_to_newest;
          t "clear resets" trail_clear_resets;
          t "zero capacity is safe" zero_capacity_is_safe;
        ] );
      ( "parity",
        [ t "the Rust transcript is reproduced" the_rust_transcript_is_reproduced ] );
    ]
