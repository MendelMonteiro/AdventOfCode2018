﻿module Puzzle1

open System
open System.IO

let puzzle1Part1 =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input1.txt") 
    |> Seq.map (fun x -> Int32.Parse(x))
    |> Seq.sum

let puzzle1Part2 = 
    let inputLines = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input1.txt") |> Array.ofSeq
    let findDuplicate (existing:Set<'a>, found) x = if existing.Contains x then (existing, Some x) else (x |> existing.Add, None)

    let result = Seq.initInfinite (fun x -> inputLines)
                 |> Seq.concat
                 |> Seq.map (fun x -> Int32.Parse(x))
                 |> Seq.scan (+) 0
                 |> Seq.scan (fun s x -> findDuplicate s x) (Set.empty, None)
                 |> Seq.filter (fun (_, x) -> Option.isSome x)
                 |> Seq.head
    
    printfn "%A" result

