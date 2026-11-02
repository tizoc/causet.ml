(* The two examples of the Rust guide page guide/scheduling.md that the
   ported tests rely on: "Faults" (a panic faults the Store, nothing is
   rolled back) and "Bug reports" (the fallback runs, the report is placed).
   Both are doctests of the Rust port. *)

open Causet

type fragile = Fine | Break

let fragiles =
  Message.make ~name:"fragile.action"
    ~kind:(function Fine -> "fragile.fine" | Break -> "fragile.break")
    ()

let glass =
  Component.make ~name:"glass" ~actions:fragiles ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      Ctx.update ctx (( + ) 1);
      match action with Break -> failwith "broke" | Fine -> ())
    ()

let faults_keep_what_the_reducer_changed () =
  let reg = Registry.create () in
  Registry.root reg glass;
  let protocol =
    match Registry.freeze reg with Ok p -> p | Error _ -> failwith "freeze"
  in
  let _sink, inbox = Event_bus.create protocol ~worker_capacity:16 in
  let store = Store.create_exn protocol ~state:0 ~services:() inbox in
  let m = Protocol.resolve_exn protocol (Path.root glass) in
  let faulted = function Error (Faulted s) -> Some s | _ -> None in
  Alcotest.(check (option string))
    "first" (Some "broke")
    (faulted (Store.dispatch_to store m Break));
  Alcotest.(check (option string))
    "later" (Some "broke")
    (faulted (Store.dispatch_to store m Fine));
  Alcotest.(check int) "the reducer's change before the panic stays" 1 (Store.state store);
  Alcotest.(check (option string)) "fault" (Some "broke") (Store.fault store)

type shelf = { titles : string list; index : (string * int) list }
type take = Take of string

let takes : take Message.t =
  Message.make ~name:"shelf.action" ~kind:(fun (Take _) -> "shelf.take") ()

(* A helper with no context: the report finds its Store by itself. *)
let take_title shelf title =
  match List.assoc_opt title shelf.index with
  | None -> (shelf, None)
  | Some at -> (
      let shelf = { shelf with index = List.remove_assoc title shelf.index } in
      match List.nth_opt shelf.titles at with
      | Some found -> (shelf, Some found)
      | None ->
          Bug.condition
            ~fields:[ ("title", title); ("at", string_of_int at) ]
            __POS__ "the index names a title the shelf does not have";
          (shelf, None)
      (* the fallback: as if it were not there *))

let shelves =
  Component.make ~name:"shelves" ~actions:takes ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx (Take title) _ -> Ctx.update ctx (fun s -> fst (take_title s title)))
    ()

let a_bug_report_lets_the_store_run_on () =
  let reg = Registry.create () in
  Registry.root reg shelves;
  let protocol =
    match Registry.freeze reg with Ok p -> p | Error _ -> failwith "freeze"
  in
  let _sink, inbox = Event_bus.create protocol ~worker_capacity:16 in
  let broken = { titles = []; index = [ ("Dune", 3) ] } in
  let store = Store.create_exn protocol ~state:broken ~services:() inbox in
  let log = Observe.Bug_log.create () in
  Store.add_observer store (Observe.Bug_log.observer log);
  let m = Protocol.resolve_exn protocol (Path.root shelves) in
  Alcotest.(check bool)
    "the fallback ran" true
    (Store.dispatch_to store m (Take "Dune") = Ok Executed);
  match Observe.Bug_log.reports log with
  | [ report ] ->
      let place = Option.get report.place in
      Alcotest.(check (pair string string))
        "place" ("Reducer", "/")
        (Bugs_common.phase_string place.phase, place.path);
      Alcotest.(check (pair string string)) "field" ("at", "3") (List.nth report.fields 1)
  | rs -> Alcotest.failf "expected one report, got %d" (List.length rs)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "guide_faults"
    [
      ( "guide",
        [
          t "faults keep what the reducer changed" faults_keep_what_the_reducer_changed;
          t "a bug report lets the store run on" a_bug_report_lets_the_store_run_on;
        ] );
    ]
