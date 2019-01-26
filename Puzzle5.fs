module Puzzle5

open System
open System.IO
open Shared

let inputFile = File.ReadAllText(__SOURCE_DIRECTORY__ + "\input5.txt") 
let test = "dabAcCaCBAcCcaDA"
let input = inputFile

type Polarity = Positive | Negative
type PolymerUnit = { Value: char; Polarity: Polarity }
let toPolymer c = { Value = Char.ToLowerInvariant c; Polarity = if Char.IsLower c then Positive else Negative }

let combinePolymers polymers unit = 
    let canCombine x y = (x.Value = y.Value) && (x.Polarity <> y.Polarity)
    match polymers with
    | head::tail when canCombine unit head -> tail 
    | xs -> unit::xs
    | [] -> [unit]

let collapsePolymers = List.fold combinePolymers [] >> List.length 

let puzzle5Part1 =
    let collapse = explode >> List.map toPolymer >> collapsePolymers
    input |> collapse

let puzzle5Part2 =
    let collapseRemoving list = 
        let polymers = list |> explode |> List.map toPolymer
        let areTheSameType c x = c.Value = x.Value
        let removePolymer c = List.filter (areTheSameType c >> not) 
        let removePolymers = Seq.map (fun c -> removePolymer c polymers)
        seq { for x in 'a'..'z' do yield toPolymer x } 
        |> removePolymers
        |> Seq.map collapsePolymers
        |> Seq.min
    input |> collapseRemoving

[<EntryPoint>]
let main argv =
    let x = puzzle5Part1 
    printfn "%A" x
    0
