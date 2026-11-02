The causet guide's OCaml examples for its projections page, run by mdx
(`dune test doc`). The guide's pages are in its own repository and show
each block here by the name above it.

<!-- snippet: bursts -->
```ocaml
open Causet_projection
module By_source = Map.Make (String)

let minute = 60_000

(* A stored report: a source failed, at an event time. *)
type failed = { at : int; source : string }

(* A source failed three times or more within a minute. *)
type burst = { key : string; failures : int }

let bursts () =
  (* The newest event time seen: records can arrive late, so the window is
     measured from here, never from the record at hand. *)
  let newest = ref 0 in
  (* Each source's failures within the window. *)
  let recent = ref By_source.empty in
  projection ~name:"bursts" ~retention_ms:minute (fun failed ->
      newest := max !newest failed.at;
      let times = failed.at :: Option.value (By_source.find_opt failed.source !recent) ~default:[] in
      recent := By_source.add failed.source times !recent;
      let failures = List.length (List.filter (fun t -> t <= failed.at && t + minute >= failed.at) times) in
      let out =
        if failures >= 3 then [ { key = Printf.sprintf "%s@%d" failed.source failed.at; failures } ]
        else []
      in
      (* Keep only the window: what a warm-up would feed back. *)
      let horizon = max 0 (!newest - minute) in
      let keep _ times = match List.filter (fun t -> t >= horizon) times with [] -> None | kept -> Some kept in
      recent := By_source.filter_map keep !recent;
      out)

let fresh make () =
  let projector = Projector.create ~time:(fun failed -> failed.at) in
  Projector.add projector (make ());
  projector

let log =
  List.map (fun (at, source) -> { at; source })
    [ (0, "db"); (40_000, "db"); (70_000, "db"); (80_000, "cache"); (100_000, "db") ]

let () =
  let projector = fresh bursts () in
  let out = List.map (Projector.process projector) log in
  (* At 70 s, the failure at 0 is more than a minute old: only at 100 s are
     there three within a minute. *)
  assert (out = [ []; []; []; []; [ { key = "db@100000"; failures = 3 } ] ])
```

<!-- snippet: warm-up -->
```ocaml
let () =
  (* The process restarts with its cursor past the first four records, the
     latest of them at 80 s. *)
  let stored = List.filteri (fun i _ -> i < 4) log and next = List.nth log 4 in
  let projector = fresh bursts () in
  (* A minute of context: read the stored log from 20 s on, and feed it back
     in log order. What the projection derives meanwhile is dropped. *)
  let from = Option.get (Projector.warm_up_from projector 80_000) in
  assert (from = 20_000);
  List.iter (Projector.warm projector) (List.filter (fun failed -> failed.at >= from) stored);
  (* The failures at 40 s and 70 s are back in context. *)
  assert (Projector.process projector next = [ { key = "db@100000"; failures = 3 } ]);
  (* The same, for a log held in memory. *)
  let projector = fresh bursts () in
  Projector.warm_up projector stored;
  assert (List.length (Projector.process projector next) = 1)
```

<!-- snippet: check-restarts -->
```ocaml
(* The planted bug: it reads a minute back, but says it keeps 30 seconds. *)
let short () = { (bursts ()) with name = "short bursts"; retention_ms = Some 30_000 }

let () =
  (* Restart before every record but the first, one after another. *)
  let restarts = List.init (List.length log - 1) succ in
  assert (check_restarts (fresh bursts) log restarts = Ok ());
  (* Restarted at 80 s, the short one warms up from 50 s on, and misses the
     failure at 40 s. *)
  assert (
    check_restarts (fresh short) log restarts
    = Error { restart = Some 4; record = 4; expected = [ { key = "db@100000"; failures = 3 } ]; actual = [] })
```

<!-- snippet: check-incremental -->
```ocaml
module Int_map = Map.Make (Int)

let minute = 60_000

type seen = { at : int; id : int }

(* How many times an id was seen within a minute of the newest sighting. *)
type recent = { key : string; count : int }

type counts = {
  mutable newest : int;
  mutable sightings : int list Int_map.t;
  careless : bool;  (** The planted bug: recomputes only the record's own id. *)
}

let count c id = List.length (List.filter (fun at -> at + minute >= c.newest) (Int_map.find id c.sightings))
let counts c = Int_map.mapi (fun id _ -> count c id) c.sightings

let process c s =
  let before = counts c in
  c.newest <- max c.newest s.at;
  c.sightings <- Int_map.update s.id (fun l -> Some (Option.value l ~default:[] @ [ s.at ])) c.sightings;
  (* Every id whose count this record changed. *)
  Int_map.fold
    (fun id n out ->
      let judged = (not c.careless) || id = s.id in
      if judged && Int_map.find_opt id before <> Some n then { key = string_of_int id; count = n } :: out
      else out)
    (counts c) []
  |> List.rev

(* From scratch: the reference's count for the key. *)
let recompute reference key =
  let id = int_of_string key in
  if Int_map.mem id reference.sightings then Some { key; count = count reference id } else None

let log = List.map (fun (at, id) -> { at; id }) [ (0, 1); (10_000, 2); (70_000, 2) ]

let check_counts ~careless =
  let fresh careless = { newest = 0; sightings = Int_map.empty; careless } in
  let projection = fresh careless and reference = fresh false in
  Causet_projection.check_incremental
    ~key:(fun (r : recent) -> r.key)
    ~projection:(process projection)
    ~reference:(fun s -> ignore (process reference s))
    ~recompute:(recompute reference) log

let () =
  assert (check_counts ~careless:false = Ok ());
  match check_counts ~careless:true with
  | Error (m : recent Causet_projection.mismatch) ->
      (* At 70 s, id 1's sighting at 0 is past the minute: it is stored as 1. *)
      assert ((m.m_record, m.key, m.stored.count, m.recomputed.count) = (2, "1", 1, 0))
  | Ok () -> assert false
```
