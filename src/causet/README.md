# Runtime internals

`causet.ml` is the application facade. `kernel.mli` fixes the public kernel
API; `kernel.ml` assembles its implementation from private modules:

| Module | Responsibility |
| --- | --- |
| `kernel_types` | Shared representations and mutually recursive types. |
| `kernel_readiness`, `kernel_bus` | Event routing, backpressure, held delivery and notifications. |
| `wakeup` | Native socket wakeups for blocking readiness and the threaded Timer, including Windows. |
| `kernel_observe` | Observer boundaries, bug reports and fault locations. |
| `kernel_state` | State and Service projections through the mounted tree. |
| `kernel_schedule` | Admission, execution metadata and the mixed FIFO. |
| `kernel_context` | Reducer capabilities and Action-only adapter ports. |
| `kernel_registry` | Erased bindings, registration and frozen mount paths. |
| `kernel_effect` | Effect capabilities and recording a root fault. |
| `kernel_store` | Store ownership, root boundaries and invariants. |
| `kernel_harness` | A component reducer without a mounted Store. |

The shared recursive types stay together, rather than acquiring another
representation or runtime indirection to break source dependencies. The
modules import the shared types and only the implementation boundaries they
need. Public applications continue to use `Causet`, not the private kernel
modules. Start at `Store.dispatch_to` or `Store.drain_events` and follow
`kernel_schedule`; integration tests exercise that public facade.
