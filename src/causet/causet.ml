(* A framework for event-driven systems as composable state machines, with
   deterministic simulation built in. *)

module Message = Message
module Debug = Debug
module Meta = Meta
module Observe = Observe

(* Bug reports: [Bug.condition __POS__ "the index %d is past the list" i]
   reports a state the code believes impossible, and the caller carries on
   with its fallback. Inside a Store's root input the report reaches its
   observers at an exact place in the run; elsewhere, the hook. *)
module Bug = struct
  let condition = Bug.condition

  (* Where reports made outside any Store go; by default, standard error. *)
  let set_hook = Bug.set_hook
  let default_hook = Bug.default_hook
end

module Clock = Clock
module Xxh3 = Xxh3
module Timer = Timer
module Pending = Pending
include Ids
include Kernel

type never = Message.never

module Lens = struct
  type ('p, 'c) t = ('p, 'c) Kernel.lens = { get : 'p -> 'c option; set : 'p -> 'c -> 'p }

  (* A plain field. *)
  let field ~get ~set = { get = (fun p -> Some (get p)); set }

  (* A field that may be absent: [get] finds it or not, [set] writes it. *)
  let opt ~get ~set = { get; set }

  (* The whole value: a child that shares its parent's State. *)
  let id = { get = Option.some; set = (fun _ c -> c) }
end

module Child = struct
  type ('p, 'cs, 'csvc, 'ca, 'ce, 'co) t = ('p, 'cs, 'csvc, 'ca, 'ce, 'co) Kernel.child

  (* A slot named [name] holding [comp]: the child's path is its parent's
     path plus [/name]. *)
  let make = Kernel.child
  let name = Kernel.child_name
end

module Component = struct
  type ('s, 'svc, 'a, 'e, 'o) t = ('s, 'svc, 'a, 'e, 'o) Kernel.component

  let make = Kernel.component
  let name = Kernel.component_name
end
