# Examples

Run commands from the repository root. Start with the shop, then follow
the example closest to the problem you are building.

| Example | What to read it for |
| --- | --- |
| [Shop](shop/README.md) | Start here: Actions, Outputs, Effects, Events, and reordered replies. |
| [P2P](p2p/README.md) | A reusable component with transport Services and connection identities. |
| [P2P node](p2p_node/README.md) | Mount P2P under a host that owns policy and request routes. |
| [Wallet send](wallet_send/README.md) | Study a larger workflow: pages, snapshot pins, retries and hydration. |
| [Chain placement](chain_placement/README.md) | Project stored reports; check incremental updates and restarts. |

```sh
dune exec examples/shop/main.exe
```

The shop prints:

```text
before delivery: pending=1, paid=[]
after delivery: pending=0, paid=[4]
simulation: declined=[200], paid=[30]
```

Its in-memory gateway answers through the Event bus and signals a persistent
readiness notification; only the driver drains the reply. The second half runs
the same Payments component under simulation and delivers answers in the
opposite order. The larger examples are reference applications exercised
through tests, including generated scenarios; their READMEs give the entry
points and commands. They use simplified domain models and simulated
Services.

The [guide examples](../doc/) cover composition, timers and reporting in smaller steps,
and run as part of the normal test suite.
