open System
open FSharp.Data
open Investments
open Load
open Calculations

[<EntryPoint>]
let main argv = 
    let securities = Load.securitiesMap
    let trades = Load.trades securities
//    let tradesBySec = trades |> List.groupBy (fun (t,p) -> t.Security.Symbol)
//    let investments = tradesBySec |> 
//        List.iter
//            (fun trade ->
//                let (ti,_) = trade
//                printfn "%A %A %A %A" ti.Security ti.Date ti.Quantity ti.UnitPrice)
//            trades
    npv DateTime.Today 5.0 trades
    |> printf "%A"
    0