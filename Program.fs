
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

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0
