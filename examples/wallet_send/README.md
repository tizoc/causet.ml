# Wallet send on a light node

A larger reference application combining bounded input discovery,
snapshot-pinned requests, retries, cancellation, proving and hydration.
It is useful after the shop and P2P examples.

Read in this order:

1. [`state.ml`](state.ml): the send lifecycle and its transition helpers.
2. [`send.ml`](send.ml): the component and its ordered emissions.
3. [`node.ml`](node.ml): context, child integration and I/O ownership.
4. `test/`: focused transitions, deadlines, probe lifecycle, then the
   simulated world and generated scenarios.

From the repository root:

```sh
dune exec examples/wallet_send/test/transitions.exe
dune test examples/wallet_send
```

The generated scenarios run a light node and up to three full peers,
including faulty, late and restarting peers. Ledger data and Services are
modelled for the example; it does not connect to a blockchain or run a real
prover. Success is a passing test suite.
