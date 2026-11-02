(* The tree the scheduling page's root-guard block runs on, which the page
   does not show: a node with its peers, a chain at [chain], and a sync at
   [sync] under it, two levels below the root. *)

type sync_state = { asked : int list }
type sync_action = Request of { peer : int } [@@deriving message ~name:"sync.action"]

let sync : (sync_state, unit, sync_action, never, never) Component.t =
  Component.make ~name:"sync" ~actions:sync_action_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx (Request { peer }) _ ->
      Ctx.update ctx (fun s -> { asked = s.asked @ [ peer ] }))
    ()

type chain_state = { sync : sync_state }
type chain_action = Synced [@@deriving message ~name:"chain.action"]

let sync_slot = Child.make "sync" sync

let chain : (chain_state, unit, chain_action, never, never) Component.t =
  Component.make ~name:"chain" ~actions:chain_action_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun _ Synced _ -> ())
    ~register:(fun r ->
      Registrar.child r sync_slot
        ~lens:(Lens.field ~get:(fun c -> c.sync) ~set:(fun _ sync -> { sync }))
        ())
    ()

type node_state = { peers : int list; chain : chain_state }
type node_action = Connect of int [@@deriving message ~name:"node.action"]

let chain_slot = Child.make "chain" chain

let node : (node_state, unit, node_action, never, never) Component.t =
  Component.make ~name:"node" ~actions:node_action_message ~effects:Message.never
    ~outputs:Message.never
    ~reduce:(fun ctx (Connect peer) _ ->
      Ctx.update ctx (fun s -> { s with peers = s.peers @ [ peer ] }))
    ~register:(fun r ->
      Registrar.child r chain_slot
        ~lens:(Lens.field ~get:(fun n -> n.chain) ~set:(fun n chain -> { n with chain }))
        ())
    ()

let empty_node = { peers = []; chain = { sync = { asked = [] } } }
