(* Execution metadata. *)

open Ids

(* Who emitted an item: the execution, the component (an inline child's
   emissions carry the child), and the item's position among the
   emissions. *)
type origin = {
  causal : execution_id;
  component : instance_id;
  emission_ordinal : int;
      (** Its position in the emitting execution's emissions, inline children's included.
          A refused queued Action or an ignored Output leaves its gap. *)
  lowering_ordinal : int;
      (** Its position among the Actions one Output was lowered to; 0 otherwise. *)
}

type t = {
  id : execution_id;
      (** Its number in the Store's run: 1, 2, 3, ... as executions start. *)
  depth : int;  (** Active frames including this one: a root input is at depth 1. *)
  time : timestamp;
      (** The Store's clock as it started, never before the execution before it. Reducers
          read time here. *)
  origin : origin option;  (** [None] for a root input, which is self-caused. *)
}

(* The execution that caused this one; its own ID for a root input. *)
let causal_execution_id m = match m.origin with None -> m.id | Some o -> o.causal

(* The kind of an execution. Outputs are lowered in place and are never
   executions. *)
type category = Action | Effect | Event

let category_name = function Action -> "action" | Effect -> "effect" | Event -> "event"
