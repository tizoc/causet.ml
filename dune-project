(lang dune 3.18)

(using mdx 0.4)

(name causet)

(generate_opam_files true)

(license Apache-2.0)

(authors "Bruno Deferrari")

(maintainers "Bruno Deferrari <utizoc@gmail.com>")

(source (github tizoc/causet.ml))

(documentation "https://tizoc.github.io/causet/")

(package
 (name causet)
 (synopsis
  "A framework for event-driven systems as composable state machines, with deterministic simulation built in")
 (description
  "Causet structures asynchronous software as a tree of components that talk through four kinds of message: Actions change State through reducers, Effects ask Services for work, Events bring the answers back, and Outputs report to a parent. A Store runs one input at a time in a fixed order, with time from its own clock, so the same components run against the real world or a simulated one, and a test can repeat a run exactly. The package includes a test library (held Events, virtual time, invariants, generated runs shrunk to small repeatable cases), observers, projections of ordered logs, and a deriver for message types. The reporting pipeline is the package causet-reporting.")
 (tags (state-machine simulation testing deterministic event-driven))
 (depends
  (ocaml (>= 5.3))
  (mtime (>= 2.0))
  (ppxlib (>= 0.36))
  (logs (>= 0.7))
  (alcotest (and :with-test (>= 1.7)))
  (mdx (and :with-test (>= 2.3)))
  (causet-reporting (and :with-test (= :version)))
  (odoc :with-doc)))

(package
 (name causet-reporting)
 (synopsis
  "A reporting pipeline: deduplication, rate limits, sampling, metric aggregation, JSON-lines dumps")
 (description
  "Turns an application's error, metric, event and healthcheck reports into numbered records: deduplicated, rate limited per source, sampled by a hash of each report so the same report is always kept or dropped, and aggregated per metric over windows; then dumped as JSON lines, rotated by size. Decisions match the Rust crate causet-reporting bit for bit. It knows nothing of the causet Store: an application wires it in as a Service.")
 (tags (reporting observability metrics rate-limit))
 (depends
  (ocaml (>= 5.3))
  (yojson (>= 2.0))
  (alcotest (and :with-test (>= 1.7)))))
