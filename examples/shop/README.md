# Shop

The first runnable example: a cart lowers its Output into a payment, whose
Service answers through the Event bus. A second run puts the same Payments
component in simulation and delivers two answers in reverse order.

From the repository root:

```sh
dune exec examples/shop/main.exe
```

Read [`main.ml`](main.ml), then the
[guide shop component](../../src/guide/shop.ml).
The expected output is in the [examples index](../README.md). The gateway
is an in-memory stand-in; no payment provider or network is needed.
The workflow assertions run in the normal test suite.
