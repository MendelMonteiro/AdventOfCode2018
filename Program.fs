
open System
open System.Text.RegularExpressions
open System.IO
open System.Threading

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

let puzzle3Part1 =
    (*
    One or more corners is inside another rectangle
    Points impacted calculated as: 
      - compeletely inside = size of rect
      - top right = my right x - max(other left x, my left x), my top y - max(other bottom y, my bottom y)
      - bottom right = my right x - max(other left x, my left x), min(other top y, my top y) - my bottom y 
      - bottom left = my left x - min(other right x, my right x), min(other top y, my top y) - my bottom y 
      - top left = my left x - max(other right x, my right x), my top y - max(other bottom y, my bottom y)
    Do for all rectangles, aggregating points into a set

    let rectPoints (x, y) = [x; y] 
    let allPoints = Array.map rectPoints >> List.concat 
    let maxPoints (maxX, maxY) (x, y) = (max maxX x, max maxY y)
    let bounds = List.fold maxPoints (0, 0)
    input |> rectangles |> allPoints |> bounds

    *)
    let inputFile = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input3.txt") |> Array.ofSeq
    let test = [|"#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2"|]
    let input = inputFile
    let parseRectangle s = 
        let result = Regex.Match(s, "\#\d+ @ (\d+),(\d+)\: (\d+)x(\d+)")
        let p = Int32.Parse
        let (left, top) = (p result.Groups.[1].Value, p result.Groups.[2].Value)
        let (width, height) = (p result.Groups.[3].Value, p result.Groups.[4].Value)
        ((left, top), (left + width - 1, top + height - 1))

    let rectangles = Array.map parseRectangle 
    let usedPointInRect ((x1, y1), (x2, y2)) = 
        let columns = [x1..x2]
        let rows = [y1..y2]
        List.allPairs columns rows
        
    let usedPoints = Seq.map usedPointInRect >> Seq.concat
    let moreThanOne (_, xs) = (xs |> Seq.length) > 1
    let duplicates = Seq.groupBy id >> Seq.filter moreThanOne
    input |> rectangles |> usedPoints |> duplicates |> Seq.length

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0
    