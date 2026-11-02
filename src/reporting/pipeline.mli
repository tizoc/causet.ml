(** The pipeline that turns reports into records: dedupe, rate limits, sampling and metric
    aggregation. Which reports it keeps, and which samples a reservoir holds, are decided
    by BLAKE3 hashes of the reports, as in the Rust port, so both keep the same ones. It
    knows nothing of the state machine: an application wires it in as a Service. *)

type t

val create : ?config:Config.t -> unit -> t
(** A pipeline with nothing seen yet, and a run ID from the clock. Metric policies are
    indexed here: a later change to [metrics] has no effect. *)

val config : t -> Config.t

val set_config : t -> Config.t -> unit
(** Read on every report from then on, except its metric policies. *)

val stats : t -> Report.Stats.t
val is_enabled : t -> bool

val run_id : t -> string
(** Tells this pipeline's records from another run's: the nanoseconds since the Unix epoch
    at [create], in base62. Only the first record carries it. *)

val accept_report : t -> int -> Report.t -> Report.outcome
(** Feeds the report to its metric policy's aggregation, then runs it through dedupe, the
    rate limit and sampling at [now_ms], counting the outcome. The aggregate of a window
    the report closed skips sampling. The report's hashes are computed only when a step
    reads them: dedupe (the dedupe key's hash, or the payload's), sampling at a rate
    strictly between 0 and 1, and a full reservoir (the payload's). *)

val accept_aggregate_report : t -> int -> Report.t -> Report.Record.t option
(** Accepts a report the pipeline made itself, such as a flushed aggregate, without
    sampling or aggregating it. *)

val flush_aggregates : t -> int -> Report.t list
(** A [metric_aggregate] report for each window that has lasted its [window_ms] by
    [now_ms] and holds data, in source and event order, as in the Rust port; each such
    window starts again at [now_ms]. None while disabled. The reports are not records yet:
    pass them to {!accept_aggregate_report}. *)

val threshold : float -> int64
(** The bound, an unsigned 64-bit integer, that a report's hash must not pass to be kept
    at [rate], strictly between 0 and 1. Computed in [f32], as in the Rust port. *)
