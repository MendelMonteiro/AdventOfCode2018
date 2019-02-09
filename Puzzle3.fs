module Puzzle3

open System
open System.Text.RegularExpressions
open System.IO

[<Measure>] type hor 
[<Measure>] type ver 
[<Measure>] type area = hor * ver
type Claim = { Id: int; TopLeft: (int<hor> * int<ver>); BottomRight: (int<hor> * int<ver>); Size: int<area> }
let parseClaim s = 
    let result = Regex.Match(s, "\#(\d+) @ (\d+),(\d+)\: (\d+)x(\d+)")
    let p = Int32.Parse
    let pHor = Int32.Parse >> LanguagePrimitives.Int32WithMeasure
    let pVer = Int32.Parse >> LanguagePrimitives.Int32WithMeasure
    let id = p result.Groups.[1].Value
    let (left, top) = (pHor result.Groups.[2].Value, pVer result.Groups.[3].Value)
    let (width, height) = (pHor result.Groups.[4].Value, pVer result.Groups.[5].Value)
    {Id=id; TopLeft=(left, top); BottomRight=(left + width - 1<hor>, top + height - 1<ver>); Size=width*height}

let claims = Array.map parseClaim 
let usedPointInClaim claim =
    let (left, top) = claim.TopLeft
    let (right, bottom) = claim.BottomRight
    seq { for col in left..1<hor>..right do for row in top..1<ver>..bottom do yield ((col, row), claim) } |> List.ofSeq
    
let usedPoints = Array.map usedPointInClaim >> List.concat
let hasCount f = Seq.length >> f

let puzzle3Part1 =
    let inputFile = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input3.txt") |> Array.ofSeq
    let test = [|"#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2"|]
    let input = inputFile
    let moreThanOne x = x > 1
    let contested = List.groupBy fst >> List.filter (snd >> hasCount moreThanOne)
    input |> claims |> usedPoints |> contested |> Seq.length

let puzzle3Part2 = 
    let inputFile = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input3.txt") |> Array.ofSeq
    let test = [|"#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2"; |]
    let input = inputFile
    let onlyOne x = x = 1
    let sameAsClaim (c:Claim) (x:int) = x = int c.Size
    let filterClaim (c, xs) = xs |> List.length |> sameAsClaim c
    let uncontested = List.groupBy fst >> List.filter (snd >> hasCount onlyOne) >> List.map snd >> List.concat >> List.groupBy snd >> List.filter filterClaim
    let lines = input |> claims |> usedPoints 
                      |> uncontested |> List.map fst |> List.map (fun x -> x.Id)
    lines

