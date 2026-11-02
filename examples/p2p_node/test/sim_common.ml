(* Shared counters, clocks and identity helpers for generated runs. *)

open P2p
open P2p_node
module Harness = Causet_test.Node
module Rng = Causet_test.Rng
module Swarm = Causet_test.Swarm
module Group = Causet_test.Group
module Scenario = Causet_test.Scenario
module In_flight = Causet_test.In_flight
module N = Node_p2p

let ms = 1_000_000
let start = 1_000 * ms

(* Counted over the whole check, for the floors at its end. *)
let offered = ref 0
let refused = ref 0
let answered = ref 0
let bounces = ref 0
let rebound = ref 0
let retries = ref 0
let retrying = ref 0
let overtaken = ref 0
let kept = ref 0
let callbacks = ref 0
let callbacks_refused = ref 0
let intents = ref 0
let intents_refused = ref 0

module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)

module End_map = Map.Make (struct
  type t = peer_id * connection_id

  let compare = compare
end)

module Request_set = Set.Make (struct
  type t = int * peer_id * wire_id

  let compare = compare
end)

module Want_set = Set.Make (struct
  type t = int * peer_id

  let compare = compare
end)
