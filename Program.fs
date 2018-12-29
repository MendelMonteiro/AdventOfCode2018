
open System
open System.IO

let partOne =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input1.txt") 
    |> Seq.map (fun x -> Int32.Parse(x))
    |> Seq.sum


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0
