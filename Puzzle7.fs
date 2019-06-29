module Puzzle7

open System.Text.RegularExpressions
open System.IO
open Shared

let test = 
    [|"Step C must be finished before step A can begin.";
      "Step C must be finished before step F can begin.";
      "Step A must be finished before step B can begin.";
      "Step A must be finished before step D can begin.";
      "Step B must be finished before step E can begin.";
      "Step D must be finished before step E can begin.";
      "Step F must be finished before step E can begin."|]

let inputFile = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input7.txt") 
let input = inputFile;

type NodeIdentifier = char
type Vertex = {FromNode: NodeIdentifier; ToNode: NodeIdentifier}
type Node = {Id: NodeIdentifier; Nodes: List<NodeIdentifier>; Parents: List<NodeIdentifier>}

let puzzle7Part1 =
    let buildVertex line = 
        let m = Regex.Match(line, @"Step (.) must be finished before step (.) can begin.") 
        {FromNode = m.Groups.[1].Value.[0]; ToNode = m.Groups.[2].Value.[0]}

    let buildGraph2 vertices =
        let buildMapOfAllNodes (map:Map<NodeIdentifier, Node>) vertex =
            let toNode = match Map.tryFind vertex.ToNode map with | Some n -> n | None -> { Id = vertex.ToNode; Nodes = List.empty; Parents = List.empty }
            let toNodeWithParents = {toNode with Parents = vertex.FromNode :: toNode.Parents} 
            let fromNode = match Map.tryFind vertex.FromNode map with | Some n -> n | None -> { Id = vertex.FromNode; Nodes = List.empty; Parents = List.empty }
            let fromNodeWithNodes = {fromNode with Nodes = vertex.ToNode :: fromNode.Nodes} 
            let mapWithFrom = Map.add vertex.FromNode fromNodeWithNodes map
            Map.add vertex.ToNode toNodeWithParents mapWithFrom
        let findRoots (m:Map<NodeIdentifier, Node>) = (m |> Seq.filter (fun x -> List.isEmpty x.Value.Parents) |> Seq.map (fun x -> x.Value))
        let allNodes = vertices |> Array.fold buildMapOfAllNodes Map.empty
        (findRoots allNodes, allNodes) 

    let (roots, allNodes) = input |> Array.map buildVertex |> buildGraph2 

    let rec traverseGraph allNodes currentNode pendingNodes processedNodes = 
        // 1. Print current node
        // 2. Add children to pending
        // 3. Check if children have all parents already processed
        // 3. Sort pending
        // 4. Call recursively for first pending
        // 5. Stop when pending is empty
        let newProcessedNodes = currentNode.Id :: processedNodes
        let findNode id = Map.find id allNodes
        let isProcessed id = List.contains id newProcessedNodes
        let allParentsAreProcessed n = n.Parents |> List.isEmpty || n.Parents |> List.forall isProcessed
        let newPending = currentNode.Nodes |> List.map findNode |> List.where allParentsAreProcessed
        let getId n = n.Id
        let allPending = List.concat [pendingNodes; newPending] |> List.sortBy getId
        match allPending with
        | x :: xs -> traverseGraph allNodes x xs newProcessedNodes
        | [] -> newProcessedNodes |> List.rev

    let traverseGraphMultipleRoots allNodes rootNodes processedNodes = 
        let sortedRoots = rootNodes |> Seq.sort |> List.ofSeq
        let firstRoot = List.head sortedRoots
        let pending = List.tail sortedRoots |> List.map (fun x -> allNodes |> Map.find x.Id)
        traverseGraph allNodes firstRoot pending processedNodes
        
    let result = traverseGraphMultipleRoots allNodes roots []

    printfn "%A" (result |> implode)
