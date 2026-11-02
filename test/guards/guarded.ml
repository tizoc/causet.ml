(* The tree the root-guard tests and the cross-port conformance trace
   ([test_conformance.ml], against [conformance/guards.txt]) share, the Rust
   port's [tests/guarded/mod.rs]: / -> mid -> leaf -> tip, a second mount
   of Mid at /other and a third at /spare. The leaf under /mid has a root guard, [may_add],
   and the one under /other another, [other_may_add]. *)

open Causet

(* ------------------------------------------------------------------------ *)
(* Tip: reports what it is asked, as an Output its parent lowers. *)

type tip_action = Report of int [@@deriving message ~name:"tip.action"]
type tip_output = Reported of int [@@deriving message ~name:"tip.output"]

let tip : (unit, unit, tip_action, never, tip_output) Component.t =
  Component.make ~name:"tip" ~actions:tip_action_message ~effects:Message.never
    ~outputs:tip_output_message
    ~reduce:(fun ctx (Report n) _ -> Ctx.emit_output ctx (Reported n))
    ()

(* ------------------------------------------------------------------------ *)
(* Leaf: adds numbers, and reaches [Add] by every route that enters a mount.
   Its own check refuses 100 and more. *)

type leaf_state = { total : int }

type leaf_action =
  | Add of int
  | Again of int  (** Queues [Add] for itself. *)
  | Echo of int  (** [Add] from its Effect handler. *)
  | Ask of int  (** [Add] lowered from its child's Output. *)
[@@deriving message ~name:"leaf.action"]

(* Named apart from the Action it carries out; its kind is the Rust port's. *)
type leaf_effect = Echo_effect of int [@kind "leaf.echo"] [@debug_name "Echo"]
[@@deriving message ~name:"leaf.effect"]

type bumped = Bumped of int [@@deriving message ~name:"leaf.bumped"]

let tip_slot = Child.make "tip" tip

let leaf : (leaf_state, unit, leaf_action, leaf_effect, never) Component.t =
  Component.make ~name:"leaf" ~actions:leaf_action_message ~effects:leaf_effect_message
    ~outputs:Message.never
    ~is_enabled:(fun _ action _ -> match action with Add n -> n < 100 | _ -> true)
    ~reduce:(fun ctx action _ ->
      match action with
      | Add n -> Ctx.update ctx (fun s -> { total = s.total + n })
      | Again n -> Ctx.emit_action ctx (Add n)
      | Echo n -> Ctx.emit_effect ctx (Echo_effect n)
      | Ask n -> Ctx.to_child ctx tip_slot (Report n))
    ~handle:(fun ctx (Echo_effect n) _ -> ignore (Effect_ctx.dispatch ctx (Add n)))
    ~register:(fun r ->
      Registrar.child r tip_slot
        ~lens:(Lens.field ~get:(fun _ -> ()) ~set:(fun s () -> s))
        ~on_output:(fun _ (Reported n) port -> Port.emit_action port (Add n))
        ();
      Registrar.event r bumped_message (fun (Bumped n) port ->
          Port.emit_action port (Add n)))
    ()

(* ------------------------------------------------------------------------ *)
(* Mid: forwards to its leaf inline or queued, and can drop it. Its slot
   guard refuses an addition ending in 13. *)

type mid_state = {
  leaf : leaf_state option;
  outcomes : outcome list;  (** Its inline calls', in order. *)
}

type mid_action = Forward of leaf_action | Send of leaf_action | Drop_leaf
[@@deriving message ~name:"mid.action"]

let empty_mid = { leaf = Some { total = 0 }; outcomes = [] }
let leaf_slot = Child.make "leaf" leaf

let mid : (mid_state, unit, mid_action, never, never) Component.t =
  Component.make ~name:"mid" ~actions:mid_action_message ~effects:Message.never
    ~outputs:Message.never
    ~is_enabled:(fun _ action _ ->
      match action with Forward (Add n) -> n < 1000 | _ -> true)
    ~reduce:(fun ctx action _ ->
      match action with
      | Forward action ->
          let outcome = Ctx.reduce_child ctx leaf_slot action in
          Ctx.update ctx (fun s -> { s with outcomes = s.outcomes @ [ outcome ] })
      | Send action -> Ctx.to_child ctx leaf_slot action
      | Drop_leaf -> Ctx.update ctx (fun s -> { s with leaf = None }))
    ~register:(fun r ->
      Registrar.child r leaf_slot
        ~lens:(Lens.opt ~get:(fun s -> s.leaf) ~set:(fun s l -> { s with leaf = Some l }))
        ~service:Fun.id
        ~guard:(fun _ action _ ->
          match action with Add n -> n mod 100 <> 13 | _ -> true)
        ())
    ()

(* ------------------------------------------------------------------------ *)
(* App: three mounts of Mid, and inline calls into /mid and /spare. The
   leaf under /mid is gated by [blocked] and a limit on its total, the one
   under /other by [other_blocked]. The root guard on /mid checks an
   addition forwarded through it (the forwarding rule), and App's slot guard
   on /mid refuses one ending in 31; /other has neither. /spare is absent
   unless set, and its root guard refuses everything. Mid refuses
   forwarding 1000 or more. *)

type app_state = {
  blocked : int list;
  other_blocked : int list;
  limit : int;
  mid : mid_state;
  other : mid_state;
  spare : mid_state option;
}

type app_action =
  | Block of int
  | Unblock of int
  | Mid of mid_action  (** Calls /mid inline. *)
  | Spare of mid_action  (** Calls /spare inline. *)
  | Block_then_forward of int
      (** Blocks [n], then forwards an addition of it through /mid inline: the root guard
          on /mid sees the block. *)
[@@deriving message ~name:"app.action"]

let empty =
  {
    blocked = [];
    other_blocked = [];
    limit = 0;
    mid = empty_mid;
    other = empty_mid;
    spare = None;
  }

let mid_slot = Child.make "mid" mid
let other_slot = Child.make "other" mid
let spare_slot = Child.make "spare" mid

let app : (app_state, unit, app_action, never, never) Component.t =
  Component.make ~name:"app" ~actions:app_action_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx action _ ->
      match action with
      | Block n -> Ctx.update ctx (fun s -> { s with blocked = s.blocked @ [ n ] })
      | Unblock n ->
          Ctx.update ctx (fun s ->
              { s with blocked = List.filter (fun b -> b <> n) s.blocked })
      | Mid action -> ignore (Ctx.reduce_child ctx mid_slot action)
      | Spare action -> ignore (Ctx.reduce_child ctx spare_slot action)
      | Block_then_forward n ->
          Ctx.update ctx (fun s -> { s with blocked = s.blocked @ [ n ] });
          ignore (Ctx.reduce_child ctx mid_slot (Forward (Add n))))
    ~register:(fun r ->
      Registrar.child r mid_slot
        ~lens:(Lens.field ~get:(fun s -> s.mid) ~set:(fun s mid -> { s with mid }))
        ~service:Fun.id
        ~guard:(fun _ action _ ->
          match action with Forward (Add n) -> n mod 100 <> 31 | _ -> true)
        ();
      Registrar.child r other_slot
        ~lens:(Lens.field ~get:(fun s -> s.other) ~set:(fun s other -> { s with other }))
        ~service:Fun.id ();
      Registrar.child r spare_slot
        ~lens:
          (Lens.opt ~get:(fun s -> s.spare) ~set:(fun s m -> { s with spare = Some m }))
        ~service:Fun.id ())
    ()

(* The root guard on /mid: an addition forwarded to the leaf, checked where
   it entered. *)
let mid_may_forward app _ action _ =
  match action with Forward (Add n) -> not (List.mem n app.blocked) | _ -> true

(* The root guard on /spare: it refuses everything, so a refusal as absent
   shows that availability is checked first. *)
let spare_never _ _ _ _ = false

(* The root guard on /mid/leaf. It reads the whole State and the leaf's. *)
let may_add app leaf action _ =
  match action with
  | Add n ->
      if n = 666 then failwith "a root guard that panics";
      if n = 777 then (
        Bug.condition __POS__ "a root guard's report";
        false)
      else (not (List.mem n app.blocked)) && leaf.total + n <= app.limit
  | _ -> true

let other_may_add app _ action _ =
  match action with Add n -> not (List.mem n app.other_blocked) | _ -> true

(* [mid ()] in the Rust port, named apart from the [mid] component. *)
let mid_path () = Path.child (Path.root app) mid_slot
let spare () = Path.child (Path.root app) spare_slot
let mid_leaf () = Path.child (Path.child (Path.root app) mid_slot) leaf_slot
let other_leaf () = Path.child (Path.child (Path.root app) other_slot) leaf_slot
