(* The reporting Service: the pipeline, and where its records go. *)
type reporting = {
  pipeline : Causet_reporting.Pipeline.t;
  mutable delivered : Causet_reporting.Report.Record.t list;
}

type orders_action = Fail of { order : int } [@@deriving message ~name:"orders.action"]

(* What went wrong, as data: the reducer describes it, the Service builds the
   report. *)
type orders_report = Failed of { order : int }
[@@deriving message ~name:"orders.report"]

let orders : (int, reporting, orders_action, orders_report, never) Component.t =
  Component.make ~name:"orders" ~actions:orders_action_message
    ~effects:orders_report_message ~outputs:Message.never
    ~reduce:(fun ctx (Fail { order }) _ ->
      Ctx.update ctx (( + ) 1);
      Ctx.emit_effect ctx (Failed { order }))
    ~handle:(fun ctx (Failed { order }) (meta : Meta.t) ->
      let open Causet_reporting in
      let payload =
        Payload.create "order_failed"
        |> Payload.field "order" (Payload.uint order)
        |> Payload.finish
      in
      let report = Report.make Report.Error ~source:"orders" payload in
      (* Stamped with the execution's time, in milliseconds, never the system clock. *)
      let reporting = Effect_ctx.service ctx in
      let outcome =
        Pipeline.accept_report reporting.pipeline (meta.time / 1_000_000) report
      in
      reporting.delivered <- reporting.delivered @ Option.to_list outcome.record)
    ()
