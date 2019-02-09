module Puzzle6

open System
open System.IO
open Shared

let inputFile = File.ReadAllText(__SOURCE_DIRECTORY__ + "\input6.txt") 
let test = "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"
let input = test

type Point = {X : int; Y : int}
type Coordinate = { Point : Point;  }

let puzzle6Part1 =
// Find bounds
// For each coordinate find distance to all other coordinates within bounds
// Merge all distances only keeping the smallest and marking 'tied' distances
// Exclude all distances that touch edges (as they are infinite)
// Take the remaining coordinate which has the most distances


    0

[<EntryPoint>]
let main argv =
    let x = puzzle6Part1 
    printfn "%A" x
    0
