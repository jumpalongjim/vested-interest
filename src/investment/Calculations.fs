module Calculations

open System
open Investments

let discountedValue (valueDate:DateTime) (rate:float) (flow:CashTransaction) =
    let days = float (flow.ValueDate - valueDate).Days
    let factor = (1.0 + rate) ** (days / 365.0)
    flow.Value / (decimal factor)

let npv (valueDate:DateTime) (rate:float) (flows:CashTransaction List) =
    flows
    |> List.map (discountedValue valueDate rate)
    |> List.sum