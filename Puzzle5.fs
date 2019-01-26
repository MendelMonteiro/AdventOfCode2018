module Puzzle5

open System
open System.IO

let explode (s:string) = [for c in s -> c]
let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

let inputFile = File.ReadAllText(__SOURCE_DIRECTORY__ + "\input5.txt") 
let test = "dabAcCaCBAcCcaDA"
let input = inputFile

type Polarity = Positive | Negative
type PolymerUnit = { Value: char; Polarity: Polarity }
let toPolymer c = { Value = Char.ToLowerInvariant c; Polarity = if Char.IsLower c then Positive else Negative }

let combinePolymers polymers unit = 
    let canCombine x y = (x.Value = y.Value) && (x.Polarity <> y.Polarity)
    match polymers with
    | head::tail -> if canCombine unit head then tail else unit::polymers
    | [] -> [unit]

let collapseList = List.fold combinePolymers [] >> List.length 

let puzzle5Part1 =
    let collapse = explode >> List.map toPolymer >> collapseList
    input |> collapse

let puzzle5Part2 =
    let collapseRemoving list = 
        let chars = list |> explode |> List.map toPolymer
        let isUpperOrLower c x = c.Value = x.Value
        let removeChar c = List.filter (isUpperOrLower c >> not) 
        seq { for x in 'a'..'z' do yield toPolymer x } 
        |> Seq.map (fun c -> removeChar c chars)
        |> Seq.map collapseList
        |> Seq.min
    input |> collapseRemoving

[<EntryPoint>]
let main argv =
    let x = puzzle5Part1 
    printfn "%A" x
    0
