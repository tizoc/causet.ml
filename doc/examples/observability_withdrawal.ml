type withdrawal_action =
  | Withdraw of { account : string; amount : int } [@level Info] [@fields amount]
      (** Logged at info, with its amount only: the account stays out of the logs. *)
  | Cancel
[@@deriving message ~name:"withdrawal.action" ~level:Debug]
