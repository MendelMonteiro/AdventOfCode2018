module Puzzle2

open System
open System.IO

let puzzle2Part1 =
    let inputLines = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input2.txt") |> Array.ofSeq
    let accDuplicates (two, three) (_, i) =
        match i with
        | 2 -> (1, three)
        | 3 -> (two, 1)
        | _ -> (two, three)

    let identifyDuplicate (s:string) = 
        s 
        |> List.ofSeq
        |> List.groupBy (fun x -> x) 
        |> List.map (fun (x, xs) -> (x, Seq.length xs)) 
        |> List.fold accDuplicates (0, 0)
    
    let result = 
        let multiply (x, y) = x * y
        let sum (currTwo, currThree) (two, three) = (currTwo + two, currThree + three)
        inputLines
        |> Seq.map identifyDuplicate
        |> Seq.fold sum (0, 0)
        |> multiply

    printfn "%A" result

let puzzel2Part2 = 
    let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input2.txt") |> Array.ofSeq
    let stringLength = input.[0].Length
    let areSimilar (x:string) (y:string) = 
        let matchingChars = x |> Seq.zip y
                              |> Seq.filter (fun (a, b) -> a = b)
                              |> Seq.map fst

        match matchingChars |> Seq.length with 
        | x when x = stringLength - 1 -> Some (Array.ofSeq matchingChars)
        | _ -> None

    let findSimilar = 
        input
        |> Array.map (fun x -> input |> Array.map (areSimilar x) |> Array.choose id |> Array.concat)
        |> Array.filter (fun x -> Array.length x > 0)
        |> Array.take 1
        |> Array.concat

    let result = new String(findSimilar)
    printfn "%A" result

