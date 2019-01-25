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

let puzzle5Part1 =
    let combinePolymers polymers unit = 
        let canCombine x y = (Char.ToLowerInvariant x = Char.ToLowerInvariant y) && (Char.IsLower x <> Char.IsLower y)
        match polymers with
        | head::tail -> if canCombine unit head then tail else unit::polymers
        | [] -> [unit]

    let collapse = explode >> List.fold combinePolymers [] >> List.length 
    input |> collapse

[<EntryPoint>]
let main argv =
    let x = puzzle5Part1 
    printfn "%A" x
    0
