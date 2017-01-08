module Investments

open System
open Microsoft.FSharp.Core

[<Measure>]
type Pound =
    static member ConvertToPence(x : decimal<Pound>) =
        x * 100m<Pence> / 1m<Pound>
and [<Measure>] public Pence =
    static member ConvertToPounds(x : decimal<Pence>) =
        x * 1m<Pound> / 100m<Pence>

type Security = {
    Symbol : string;
    Name : string
}

type SecurityTransaction = {
    Security: Security
    Date: DateTime;
    Quantity : decimal;
    UnitPrice : decimal<Pence>;
    Charge : decimal<Pound>
}

type CashTransaction = {
    ValueDate: DateTime;
    Value: decimal<Pound>
    }

type Transaction = 
    | Purchase of SecurityTransaction
    | Sale of SecurityTransaction
    | Fund of CashTransaction
    | CashInterest of CashTransaction
    | Dividend of SecurityTransaction

let securityTransactionValue trade =
    Pence.ConvertToPounds(trade.UnitPrice * trade.Quantity)

// Change to the cash account balance made by each type of transaction
let cashBalanceImpact t = 
    match t with
    | Purchase s -> { ValueDate = s.Date; Value = -1m * securityTransactionValue s }
    | Sale s | Dividend s -> { ValueDate = s.Date; Value = securityTransactionValue s }
    | Fund i | CashInterest i -> i

// Change to the historic cost value of an investment made by
// each type of transaction
let investmentHistoricCostImpact = function
    | Purchase t -> securityTransactionValue t
    | Sale t -> -1m * securityTransactionValue t
    | _ -> 0m<Pound>

type Investment = {
    Holding: decimal;
    HistoricCost: decimal<Pound>;
    YieldValue: decimal<Pound>;
    Fees: decimal<Pound>;
    }

let absoluteCapitalInvested i =
    let purchasedValue t = match t with | Purchase p -> investmentHistoricCostImpact t | _ -> 0m<Pound>
    List.sumBy purchasedValue i

let netCapitalCost i =
    List.sumBy investmentHistoricCostImpact i

let transactionDate = function
    | Purchase t | Sale t | Dividend t -> t.Date 
    | CashInterest m | Fund m -> m.ValueDate

    
let newBalance oBal = function
    | Purchase t -> oBal + t.Quantity
    | Sale t -> oBal - t.Quantity
    | _ -> oBal

/// Calculates the holding quantity of an investment.
let holdingQuantity investment =
    investment |> List.fold newBalance 0m

/// Calculates whether the investment is considered to be active.
let isActive investment =
    match investment with
    | [] -> false
    | _ -> holdingQuantity investment > 0m


let income investment =
    let transactionIncome = function 
        | Dividend t -> t.Quantity * t.UnitPrice 
        | _ -> 0m<Pence>
    Pence.ConvertToPounds(List.sumBy transactionIncome investment)

     
let gainOrLoss investment =
    if isActive investment
    then 0m<Pound>
    else -1m * (List.sumBy investmentHistoricCostImpact investment) + income investment

let investmentDate investment =
    let date = transactionDate (List.head investment)
    date.ToString("dd-MMM-yyyy")

let securityOfInvestment investment =
    if List.isEmpty investment
    then ""
    else
        match List.head investment with
        | Purchase t
        | Sale t -> t.Security.Symbol
        | _ -> ""

let printCompletedInvestment i =
    printfn "%s     %s   %16A   %10A" 
        (securityOfInvestment i)
        (investmentDate i) 
        (absoluteCapitalInvested i) 
        (gainOrLoss i)

let printCompletedInvestmentSummary p =
    printfn "Count: %i    Total Gain/Loss: %M"
        (Seq.length p)
        ((Seq.sumBy gainOrLoss p) / 1M<Pound>)   // Divide by 1 pound to convert pounds to simple units to satisfy print format

let reportCompletedInvestments investments =
    printfn "Security Investment date Investment value Gain/Loss"
    Seq.iter printCompletedInvestment investments
    printCompletedInvestmentSummary investments

let printActiveInvestment pi =
    let (p:decimal<Pence>), i = pi
    printfn "%s        %s %9M %11M %11M"
        (securityOfInvestment i)
        (investmentDate i)
        ((holdingQuantity i))
        ((netCapitalCost i) / 1m<Pound>)
        ((holdingQuantity i) * Pence.ConvertToPounds(p) / 1m<Pound>)

let printActiveInvestmentSummary (pis:seq<decimal<Pence> * Transaction list>) =
    printfn "Count: %i                     %11M %11M"
        (Seq.length pis)
        (pis |> Seq.sumBy (fun pi -> (netCapitalCost (snd pi)) / 1m<Pound>))
        0M

let currentPrice prices symbol =
    let securityPrice = Seq.find (fun sp -> (fst sp) = symbol) prices
    snd securityPrice

let currentPriceInvestment prices investment =
    let symbol = securityOfInvestment investment
    currentPrice prices symbol

let reportActiveInvestments investments prices =
    let pricedInvestments = investments |> Seq.map (fun i -> (currentPriceInvestment prices i),i)
    printfn "Security Investment date Holding       Cost Current Value"
    Seq.iter printActiveInvestment pricedInvestments
    printActiveInvestmentSummary pricedInvestments

let reportInvestments investments prices =
    let active, completed = List.partition isActive (Seq.toList investments)
    reportCompletedInvestments completed
    reportActiveInvestments active prices


/// Splits a list of transactions into a pair of lists, being the first investment
/// and the remainder.
let rec splitInvestment oBal = function
    | [] -> [],[]
    | head::tail ->
        let cBal = newBalance oBal head
        if cBal = 0m
        then [head], tail
        else 
            let (i,r) = splitInvestment cBal tail
            head::i, r

/// Splits a list of transactions into a list of investments
let rec splitInvestments activity =
    match activity with
    | [] -> []
    | _ ->
        let (i,r) = splitInvestment 0m activity
        match r with
        | [] -> [i]
        | _ -> i::(splitInvestments r)

let isInvestmentTransaction = function
    | Purchase _ | Sale _ | Dividend _ -> true
    | _ -> false

let groupBySecurity activity =
    let transactionSecurity = function
        | Purchase t
        | Sale t
        | Dividend t -> t.Security
        | _ -> { Symbol = ""; Name = ""}
    activity
    |> Seq.groupBy transactionSecurity
    |> Seq.map (fun g -> List.ofSeq (snd g))

let analyse activity =
    activity
    |> List.filter isInvestmentTransaction
    |> groupBySecurity
    |> Seq.map (fun a -> splitInvestments a)
    |> Seq.collect (fun a -> a)     // Builds a single collection from a sequence of collections


let accumulateInvestment inv t = 
    match t with
    | Purchase ti -> { inv with Holding = inv.Holding + ti.Quantity; Fees = inv.Fees + ti.Charge }
    | Sale ti -> { inv with Holding = inv.Holding - ti.Quantity; Fees = inv.Fees + ti.Charge }
    | Dividend ti -> { inv with YieldValue = inv.YieldValue + (cashBalanceImpact t).Value }
    | _ -> inv

//let accumulateCash oBal = function
//    | Purchase t
//    | Sale t
//    | Dividend t
//    | Fund m
//    | CashInterest m