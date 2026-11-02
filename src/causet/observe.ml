(* Observers: callbacks at each execution's start, finish and settlement, and
   at refusals, inline calls, faults and bug reports. They inspect kinds,
   causality and timing; they have no dispatch and no State authority. With
   none installed, the Store skips every callback. *)

open Ids

(* Where observers are called. *)
type boundary =
  | Start  (** When the execution starts, before its handler runs. *)
  | Finish  (** After the handler; before an Action's or Event's FIFO drains. *)
  | Settled  (** After everything an Action or Event caused. *)
  | Bus  (** The lane report at the start of a drain, outside any execution. *)
  | Refused  (** An Action refused at admission. *)
  | Inline  (** A child's Action admitted inline, before its reducer runs. *)

(* What was running when a fault happened, or a bug was reported. *)
type phase =
  | Admission  (** An enabling check or a guard. *)
  | Reducer  (** A reducer, or a child it called inline, or a lowerer. *)
  | Handler  (** An Effect handler. *)
  | Adapter  (** An Event adapter. *)
  | Clock  (** Reading the clock, before any execution exists. *)
  | Observer of boundary  (** An observer callback at this boundary. *)
  | Invariant of string  (** The named invariant found the State invalid. *)
  | Invariant_panic of string  (** The named invariant's checker raised. *)

(* One execution, as observers see it. *)
module Execution = struct
  type t = {
    category : Meta.category;
    path : string;  (** The destination mount. *)
    instance : instance_id;
    message : string;  (** The message's type name. *)
    kind : string;
    meta : Meta.t;
    answers : execution_id option;  (** For an Event, the Effect it answers. *)
  }
end

(* Where an Action was sent from. *)
type sent_from =
  | Root  (** A root input. *)
  | From_effect of execution_id  (** Dispatched by this Effect's handler. *)
  | Queued of Meta.origin  (** Queued, with the origin it would have carried. *)
  | From_inline of execution_id  (** Called inline within this execution. *)

let sent_execution = function
  | Root -> None
  | From_effect at | From_inline at -> Some at
  | Queued o -> Some o.causal

(* What refused an Action. *)
type refused_by =
  | Root_guard  (** The root guard the application set on its mount ([Registry.admit]). *)
  | Guard  (** Its parent's guard on the slot. *)
  | Component  (** Its component's own enabling check. *)
  | Absent  (** Its State is absent: the slot's lens reaches nothing. *)

(* An Action refused at admission. It has no execution. *)
module Refusal = struct
  type t = {
    path : string;
    instance : instance_id;
    message : string;
    kind : string;
    sent : sent_from;
    by : refused_by;
    time : timestamp;  (** The time it was checked at. *)
  }
end

(* A child's Action reduced inline, within its parent's execution. *)
module Inline_call = struct
  type t = {
    path : string;
    instance : instance_id;
    message : string;
    kind : string;
    within : execution_id;  (** The parent's execution. *)
    time : timestamp;
        (** The parent's execution time, which the child's reducer reads too. *)
    root_guard_skipped : bool;
        (** The child's mount has a root guard that this call did not run: an inline call
            made below the root, whose caller takes on what the guard checks
            ([Registry.admit]). *)
  }
end

(* What observers learn about a fault. *)
module Fault_info = struct
  type t = {
    phase : phase;
    at : execution_id option;  (** The innermost execution, if one was allocated. *)
    message : string;
  }
end

(* The bus at the start of a drain. *)
module Bus_depths = struct
  type t = { store_lane : int; worker_lane : int; rejected_full : int }
end

(* Where in a Store's run a bug was reported. *)
module Bug_place = struct
  type t = {
    phase : phase;
    at : execution_id option;
    component : instance_id;
    path : string;
    sequence : int;  (** Its position among all the reports of this Store's run. *)
  }
end

(* One bug report. *)
module Bug_report = struct
  type t = {
    message : string;
    fields : (string * string) list;
    file : string;
    line : int;
    place : Bug_place.t option;  (** [None] outside any Store. *)
  }
end

(* Every callback defaults to nothing ([none]). Only start, refusal and
   inline callbacks see the payload. A panic in a callback faults the Store
   as [Observer] at that boundary; one in [fault] or [bug] is ignored. *)
type t = {
  action_start : Execution.t -> Message.payload -> unit;
  action_finish : Execution.t -> unit;
  action_settled : Execution.t -> unit;
  effect_start : Execution.t -> Message.payload -> unit;
  effect_finish : Execution.t -> unit;
  event_start : Execution.t -> Message.payload -> unit;
  event_finish : Execution.t -> unit;
  event_settled : Execution.t -> unit;
  action_refused : Refusal.t -> Message.payload -> unit;
  inline_action : Inline_call.t -> Message.payload -> unit;
  fault : Fault_info.t -> unit;
  bus : Bus_depths.t -> unit;
  bug : Bug_report.t -> unit;
}

let none =
  {
    action_start = (fun _ _ -> ());
    action_finish = ignore;
    action_settled = ignore;
    effect_start = (fun _ _ -> ());
    effect_finish = ignore;
    event_start = (fun _ _ -> ());
    event_finish = ignore;
    event_settled = ignore;
    action_refused = (fun _ _ -> ());
    inline_action = (fun _ _ -> ());
    fault = ignore;
    bus = ignore;
    bug = ignore;
  }

(* Keeps every bug report, so a test can check them after a run. *)
module Bug_log = struct
  type log = { mutable reports : Bug_report.t list (* newest first *) }

  let create () = { reports = [] }
  let observer log = { none with bug = (fun r -> log.reports <- r :: log.reports) }
  let reports log = List.rev log.reports
end

(* One started execution, as [Trace_log] keeps it. *)
module Trace_entry = struct
  type t = {
    id : execution_id;
    time : timestamp;
    category : Meta.category;
    path : string;
    kind : string;
    causal : execution_id;
    origin : Meta.origin option;
    answers : execution_id option;
    depth : int;
  }
end

(* One refused Action, as [Trace_log] keeps it. *)
module Refusal_entry = struct
  type t = {
    path : string;
    kind : string;
    sent : sent_from;
    by : refused_by;
    time : timestamp;
  }
end

(* One child's Action reduced inline, as [Trace_log] keeps it. *)
module Inline_entry = struct
  type t = {
    path : string;
    kind : string;
    within : execution_id;
    time : timestamp;
    root_guard_skipped : bool;
        (** The child's mount has a root guard, which the call did not run. *)
  }
end

(* Keeps every started execution in memory, with the Actions refused and
   those reduced inline. It grows without bound. *)
module Trace_log = struct
  type log = {
    mutable entries : Trace_entry.t list;
    mutable refusals : Refusal_entry.t list;
    mutable inline : Inline_entry.t list;
  }

  let create () = { entries = []; refusals = []; inline = [] }

  let push log (e : Execution.t) =
    log.entries <-
      {
        Trace_entry.id = e.meta.id;
        time = e.meta.time;
        category = e.category;
        path = e.path;
        kind = e.kind;
        causal = Meta.causal_execution_id e.meta;
        origin = e.meta.origin;
        answers = e.answers;
        depth = e.meta.depth;
      }
      :: log.entries

  let observer log =
    {
      none with
      action_start = (fun e _ -> push log e);
      effect_start = (fun e _ -> push log e);
      event_start = (fun e _ -> push log e);
      action_refused =
        (fun (r : Refusal.t) _ ->
          log.refusals <-
            {
              Refusal_entry.path = r.path;
              kind = r.kind;
              sent = r.sent;
              by = r.by;
              time = r.time;
            }
            :: log.refusals);
      inline_action =
        (fun (i : Inline_call.t) _ ->
          log.inline <-
            {
              Inline_entry.path = i.path;
              kind = i.kind;
              within = i.within;
              time = i.time;
              root_guard_skipped = i.root_guard_skipped;
            }
            :: log.inline);
    }

  let entries log = List.rev log.entries
  let refusals log = List.rev log.refusals
  let inline log = List.rev log.inline
end
