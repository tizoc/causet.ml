# P2P node

A host for the [reusable P2P component](../p2p/README.md), with fetching and
serving consumers. It demonstrates parent-owned admission policy, request
routes and reply tokens without putting application policy into P2P.

Read [`p2p_node.ml`](p2p_node.ml) for the root and its child binding,
then the contract tests and generated simulation in `test/`.
The simulation is split into [sim_network.ml](test/sim_network.ml),
[sim_checks.ml](test/sim_checks.ml), and [generated.ml](test/generated.ml).

From the repository root:

```sh
dune exec examples/p2p_node/test/contracts.exe
dune test examples/p2p_node
```

The full suite generates two to four nodes, changes policy, loses and
delays replies, disconnects and reconnects peers, and checks identities
and eventual outcomes. Success is a passing test suite; no server remains
running. The transport and server Services are simulated.
