(* Bug reports: [Bug.condition] reports a state the code believes impossible,
   and the caller carries on with its fallback. The same in every build:
   nothing faults, nothing is cancelled or rolled back.

   While a Store runs a root input, reports wait in a buffer of the Store's
   thread; the Store takes them at fixed points, so each has an exact place
   in the run, and gives them to its observers. Elsewhere (a Service's
   thread, a plain unit test), a report goes to the process's hook. *)

open Observe

(* A report waiting for the Store, with the place it was tagged with as it
   left an inline child, a guard or a lowerer, if it did. *)
type pending = {
  message : string;
  fields : (string * string) list;
  file : string;
  line : int;
  mutable tag : (phase * int) option;
}

(* This thread's collection: open while a Store runs a root input on it. *)
type local = { mutable depth : int; mutable reports : pending list  (** newest first *) }

let thread_key () = ((Domain.self () :> int), Thread.id (Thread.self ()))
let same_thread (d, t) (d', t') = Int.equal d d' && Int.equal t t'

(* One collection per thread. Domain-local storage is shared by a domain's
   threads, so it maps each thread to its own. *)
let locals : (int * int, local) Hashtbl.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> Hashtbl.create 4)

(* A domain's threads can switch inside a table update. *)
let table_lock = Mutex.create ()

(* The collection a domain's thread used last, with its thread: one load,
   so a switch between threads cannot pair one thread with another's. *)
let last : (int * local) option ref Domain.DLS.key =
  Domain.DLS.new_key (fun () -> ref None)

let local () =
  let thread = Thread.id (Thread.self ()) in
  let last = Domain.DLS.get last in
  match !last with
  | Some (t, l) when Int.equal t thread -> l
  | _ ->
      let table = Domain.DLS.get locals in
      let key = ((Domain.self () :> int), thread) in
      let l =
        Mutex.protect table_lock (fun () ->
            match Hashtbl.find_opt table key with
            | Some l -> l
            | None ->
                let l = { depth = 0; reports = [] } in
                Hashtbl.replace table key l;
                l)
      in
      last := Some (thread, l);
      l

(* Reports waiting in any thread's scope: one load tells the Store that,
   almost always, there is nothing to take. *)
let waiting = Atomic.make 0

let default_hook (r : Bug_report.t) =
  Printf.eprintf "causet: bug condition at %s:%d: %s\n%!" r.file r.line r.message

let hook = Atomic.make default_hook

(* Where reports made outside any Store go. Replaces the previous hook;
   without one, reports are printed to standard error. *)
let set_hook f = Atomic.set hook f

let into_report p place : Bug_report.t =
  { message = p.message; fields = p.fields; file = p.file; line = p.line; place }

let report ?(fields = []) ~file ~line message =
  let p = { message; fields; file; line; tag = None } in
  let l = local () in
  if l.depth > 0 then (
    l.reports <- p :: l.reports;
    Atomic.incr waiting)
  else (Atomic.get hook) (into_report p None)

(* Reports a state the code believes impossible, then returns:
   [Bug.condition __POS__ "the index %d is past the list" i]. *)
let condition ?fields (file, line, _, _) fmt =
  Printf.ksprintf (fun message -> report ?fields ~file ~line message) fmt

let pending () = Atomic.get waiting <> 0

(* Gives the reports waiting untagged on this thread a place: they were made
   by code that has just returned (a guard, an inline child, a lowerer). *)
let tag phase mount =
  if pending () then
    List.iter
      (fun p -> if p.tag = None then p.tag <- Some (phase, mount))
      (local ()).reports

(* Runs [f], then tags what it reported, even if it raises. *)
let attribute phase mount f =
  match f () with
  | v ->
      tag phase mount;
      v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      tag phase mount;
      Printexc.raise_with_backtrace e bt

(* Takes the reports waiting on this thread, oldest first. *)
let take () =
  let l = local () in
  match l.reports with
  | [] -> []
  | reports ->
      l.reports <- [];
      ignore (Atomic.fetch_and_add waiting (-List.length reports));
      List.rev reports

(* Collects this thread's reports while [f] runs: a Store's root input, or a
   Harness reduction. A nested scope keeps its reports apart from the outer
   one's. *)
let scope f =
  let l = local () in
  let outer = l.reports in
  l.reports <- [];
  l.depth <- l.depth + 1;
  let close () =
    l.depth <- l.depth - 1;
    (* Every exit takes its reports first; anything left (a raise out of a
       Harness reducer) is dropped, and no longer counts. *)
    (match l.reports with
    | [] -> ()
    | left -> ignore (Atomic.fetch_and_add waiting (-List.length left)));
    l.reports <- outer
  in
  match f () with
  | v ->
      close ();
      v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      close ();
      Printexc.raise_with_backtrace e bt
