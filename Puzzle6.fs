module Puzzle6

open System
open System.Text.RegularExpressions
open System.IO
open Shared

let inputFile = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input6.txt") 
let test = [|"1, 1"; "1, 6"; "8, 3"; "3, 4"; "5, 5"; "8, 9"|]
let input = inputFile
[<Measure>] type distance 
let distanceLimit = 10_000<distance>

[<Measure>] type hor 
[<Measure>] type ver 
type Point = {X : int<hor>; Y : int<ver>}
type Distance = (Point * int<distance>)
type Coordinate = { Origin : Point; Distances : Distance list }
type Bounds = { Top : int<ver>; Left : int<hor>;  Bottom : int<ver>; Right : int<hor> }

let parsePoint (s: string) = 
    let groups = Regex.Match(s, "(\d+), (\d+)").Groups
    {X = groups.[1].Value |> Int32.Parse |> LanguagePrimitives.Int32WithMeasure; 
     Y = groups.[2].Value |> Int32.Parse |> LanguagePrimitives.Int32WithMeasure }
let toCoordinate x = {Origin = x; Distances = [] }
let parseCoordinates = Array.map parsePoint >> Array.map toCoordinate
let minMax b c = {Left=min b.Left c.Origin.X; Top=min b.Top c.Origin.Y; Right=max b.Right c.Origin.X; Bottom=max b.Bottom c.Origin.Y}
let findBounds = Array.fold minMax {Left = 0<hor>; Top=0<ver>; Right=0<hor>; Bottom=0<ver>}
let coordinates = input |> parseCoordinates 
let bounds = coordinates |> findBounds
let distanceToPoint c p : (Point * int<distance>) = (p, int (abs <| c.Origin.X - p.X) + int (abs <| c.Origin.Y - p.Y) |> LanguagePrimitives.Int32WithMeasure)
let allPoints b = seq { for x in b.Left..1<hor>..b.Right do for y in b.Top..1<ver>..b.Bottom do yield {X = x; Y = y} } 

let puzzle6Part1 =
    // 1. Find bounds
    // 2. For each coordinate find distance to all other coordinates within bounds
    // 3. Merge all distances only keeping the smallest and marking 'tied' distances
    // 4. Exclude all coordinates where a distance touches an edge (as they are infinite)
    // 5. Take the remaining coordinate which has the most distances 
    let minWithCount (d1:int<distance>, _:int) (d2:int<distance>, count:int) = match d2 - d1 with | x when x = 0<distance> -> (d1, count+1) | x when x < 0<distance> -> (d2, count) | _ -> (d1, 1)
    let keepSmallestDistances smallest c = c.Distances |> List.fold (fun map (point, distance) -> addOrUpdate (distance, 1) (minWithCount (distance, 1)) point map) smallest 
    let coordsWithDistances = coordinates |> Array.map (fun x -> { x with Distances = allPoints bounds |> Seq.map (distanceToPoint x) |> List.ofSeq } ) 
    let smallestDistances = coordsWithDistances |> Seq.fold keepSmallestDistances Map.empty 
    let isSmallestAndNotTied smallest (p, d) = 
        let (sd, count) = (Map.find p smallest)
        sd = d && count <= 1 
    let removeSmallest smallest c = { c with Distances = c.Distances |> List.filter (isSmallestAndNotTied smallest) }
    let touchesBounds (p, _) = p.X = bounds.Left || p.X = bounds.Right || p.Y = bounds.Top || p.Y = bounds.Bottom
    let excludeInfinite c = c.Distances |> (not << List.exists touchesBounds)
    coordsWithDistances |> Seq.map (removeSmallest smallestDistances) 
                        |> Seq.filter excludeInfinite 
                        |> Seq.map (fun c -> c.Distances.Length)
                        |> Seq.max

let puzzle6Part2 = 
    // 1. For each point calculate the distance to all the coordinates and store the sum
    // 2. Filter out all where the distance is more than 10,000 and return the count
    let distanceToCoordinate p c = distanceToPoint c p
    let calculateTotalDistance p = (p, coordinates |> Seq.map (distanceToCoordinate p) |> Seq.sumBy snd)
    let lessThanLimit (p, sum) = sum < distanceLimit
    bounds |> allPoints |> Seq.map calculateTotalDistance |> Seq.filter lessThanLimit |> Seq.length
    
[<EntryPoint>]
let main argv =
    let x = puzzle6Part2 
    printfn "%A" x
    0
