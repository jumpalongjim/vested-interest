module Load
open System
open FSharp.Data
open Investments

let [<Literal>] SecsData = __SOURCE_DIRECTORY__ + "..\\..\\..\\data\\securities.csv"
type securitiesFile = CsvProvider<SecsData>

let [<Literal>] TradesData = __SOURCE_DIRECTORY__ + "..\\..\\..\\data\\trades.csv"
type tradesFile = CsvProvider<TradesData>

let readSecurity (row : securitiesFile.Row) =
    { Symbol = row.Symbol.ToUpper(); Name = row.Name }

let securitiesMap =
    securitiesFile.Load(SecsData).Rows
    |> Seq.map readSecurity
    |> Seq.fold
       (fun acc sec -> Map.add sec.Symbol sec acc)
       Map.empty
        
// Cast decimal types to measures
let inPounds v = LanguagePrimitives.DecimalWithMeasure<Pound> v
let inPence v = LanguagePrimitives.DecimalWithMeasure<Pence> v

let readTrade (securities : Map<string, Security>) (row : tradesFile.Row) =
    let option = securities.TryFind(row.Fund.ToLower())
    match option with
    | Some s -> { Security = s; Date = row.Date; Quantity = row.Quantity; UnitPrice = inPence row.Price; Charge = 0m<Pound> }
    | None -> failwithf "No security definition found for symbol %s" row.Fund

let readTransaction (securities : Map<string, Security>) (row : tradesFile.Row) =
    match row.Transaction with
    | "Buy" -> Purchase(readTrade securities row)
    | "Sell" -> Sale (readTrade securities row)
    | "Fund" -> Fund({ ValueDate = row.Date; Value = inPounds row.Cash })

let trades securities =
    tradesFile.Load(TradesData).Rows
    |> Seq.map (readTrade securities)
    |> List.ofSeq