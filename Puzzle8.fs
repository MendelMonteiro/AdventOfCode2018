module Puzzle8

open System
open System.IO

let inputFile = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input8.txt").[0]
let test = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
let testOneChild = "1 3 0 3 10 11 12 1 1 2"
let testTwoLevels = "1 3 1 3 0 1 99 10 11 12 1 1 2"
let testOneChildAdjacent = "2 3 0 3 10 11 12 0 1 2 1 1 2"
let testOne = "0 3 1 1 2"
let input = inputFile

type Metadata = int
type Number = int
type Node = {Children: Node list; Data: Metadata list}

let toNumbers (i : string) = i.Split(' ') |> Array.map Int32.Parse 
let readMetadata m r = r |> List.take m
let skipMetadata m r = r |> List.skip m

let rec parseNode numbers currentLevelNodeCount : (Node list * Number list)=
    match numbers with
    | [] -> ([], [])
    | childNodeCount::metadataAndRest -> 
        let (metadataCount::rest) = metadataAndRest
        match (childNodeCount, metadataCount, rest, currentLevelNodeCount) with
        | 0, m, r, l when l > 1 -> // Leaf node and has adjacent nodes
            let thisNode = {Children = []; Data = readMetadata m r}
            let (adjacent, rest') = parseNode (skipMetadata m r) (l - 1)
            (thisNode::adjacent, rest')
        | 0, m, r, l when l <= 1 -> // Leaf node and no adjacent nodes
            ([{Children = []; Data = readMetadata m r}], rest |> skipMetadata m)
        | _, m, _, l when l > 1 -> // Non-leaf node and has adjacent
            let (children, rest') = parseNode rest childNodeCount
            let thisNode = {Children = children; Data = readMetadata m rest'}
            let (adjacent, rest'') = parseNode (skipMetadata m rest') (l - 1)
            (thisNode::adjacent, rest'')
        | _, m, _, l when l <= 1 -> // Non-leaf node and no adjacent
            let (children, rest') = parseNode rest childNodeCount
            ([{Children = children; Data = readMetadata m rest'}], rest' |> skipMetadata m)

let collectData node = 
    let rec traverseDataCore node = node.Data :: (List.collect traverseDataCore node.Children) 
    traverseDataCore node |> List.concat

parseNode (input |> toNumbers |> List.ofArray) 1 |> fst |> List.head |> collectData |> List.sum 