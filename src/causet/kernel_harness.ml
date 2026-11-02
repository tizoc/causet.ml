open Ids
open Observe
open Kernel_types

(* ------------------------------------------------------------------------ *)
(* The component test harness *)

(* One emission of a component under test, in emission order. *)
type ('a, 'e, 'o) emission =
  | Emit_action of 'a
  | Emit_effect of 'e
  | Emit_output of 'o
  | Inline_child of child_call  (** recorded, reported to the reducer as [Reduced] *)
  | To_child of child_call

(* Drives one component's transitions with local State and records what it
   emits, with no Protocol, Store or registration. Executions are numbered
   and stamped as a Store does it: IDs from 1, times [max previous now]. *)
module Harness = struct
  type t = {
    session : session_id;
    mutable last : timestamp;
    mutable next_id : int;
    mutable now : timestamp;
    mutable bug_reports : Bug_report.t list;
  }

  let create ?(session = 0) () =
    { session; last = 0; next_id = 1; now = 0; bug_reports = [] }

  let at h now = h.now <- now
  let next_time h = max h.last h.now

  let keep_bugs h =
    h.bug_reports <-
      h.bug_reports @ List.map (fun p -> Bug.into_report p None) (Bug.take ())

  (* The component's own enabling check, at the time the next execution
     would have. *)
  let is_enabled h comp state action =
    Bug.scope (fun () ->
        let enabled = comp.is_enabled state action (next_time h) in
        keep_bugs h;
        enabled)

  (* Runs the reducer as one root execution, skipping the enabling check, and
     returns the new State and its emissions in order. *)
  let reduce h comp state action =
    let time = next_time h in
    let meta = { Meta.id = h.next_id; depth = 1; time; origin = None } in
    h.next_id <- h.next_id + 1;
    h.last <- time;
    let collected =
      {
        c_session = h.session;
        c_execution = meta.id;
        c_next_request = 0;
        c_emissions = Queue.create ();
      }
    in
    let ctx =
      {
        cstate = state;
        cphase = Mutating;
        cmeta = meta;
        ccomp = comp;
        backend = In_harness collected;
        cmount = 0;
      }
    in
    Bug.scope (fun () ->
        comp.reduce ctx action meta;
        keep_bugs h);
    ctx.cphase <- Finished;
    let emissions =
      Queue.fold
        (fun acc raw ->
          let e =
            match raw with
            | Raw_action p -> Emit_action (P.get_exn comp.actions p)
            | Raw_effect p -> Emit_effect (P.get_exn comp.effects p)
            | Raw_output p -> Emit_output (P.get_exn comp.outputs p)
            | Raw_inline c -> Inline_child c
            | Raw_to_child c -> To_child c
          in
          e :: acc)
        [] collected.c_emissions
    in
    (ctx.cstate, List.rev emissions)

  (* One root transition as a Store runs it: the enabling check, then the
     reducer. A refusal leaves State and time alone. *)
  let dispatch h comp state action =
    if not (is_enabled h comp state action) then Error `Disabled
    else Ok (reduce h comp state action)

  let bug_reports h = h.bug_reports
end
