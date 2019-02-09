module Puzzle4

open System
open System.Text.RegularExpressions
open System.IO
open Shared

[<Measure>] type minute
[<Measure>] type day
[<Measure>] type id
type DateTime = {Day: int<day>; Minute: int<minute>}
type Entry = Start of dateTime : DateTime * guardId : int<id> | Sleep of dateTime : DateTime | WakeUp of dateTime : DateTime
type SleepTime = { GuardId: int<id>; SleepMinute: int<minute>; WakeMinute: int<minute>; }
let inputFile = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input4.txt") |> Array.ofSeq
let test = [|"[1518-11-01 00:00] Guard #10 begins shift";
                "[1518-11-01 00:05] falls asleep";
                "[1518-11-01 00:25] wakes up";
                "[1518-11-01 00:30] falls asleep";
                "[1518-11-01 00:55] wakes up";
                "[1518-11-01 23:58] Guard #99 begins shift";
                "[1518-11-02 00:40] falls asleep";
                "[1518-11-02 00:50] wakes up";
                "[1518-11-03 00:05] Guard #10 begins shift";
                "[1518-11-03 00:24] falls asleep";
                "[1518-11-03 00:29] wakes up";
                "[1518-11-04 00:02] Guard #99 begins shift";
                "[1518-11-04 00:36] falls asleep";
                "[1518-11-04 00:46] wakes up";
                "[1518-11-05 00:03] Guard #99 begins shift";
                "[1518-11-05 00:45] falls asleep";
                "[1518-11-05 00:55] wakes up"|]

let input = inputFile

let toEntry s =
    let matches = Regex.Match(s, "\d+-\d+-(\d+) \d+:(\d+)(.*)")
    let p = Int32.Parse
    let pId = p >> LanguagePrimitives.Int32WithMeasure
    let pDay = p >> LanguagePrimitives.Int32WithMeasure
    let pMinute = p >> LanguagePrimitives.Int32WithMeasure
    let guardId (s:string) = Regex.Match(s, "Guard #(\d+) begins shift").Groups.[1].Value |> pId
    let toDateTime (m:Match) = { Day = pDay m.Groups.[1].Value; Minute = pMinute m.Groups.[2].Value }
    let toType (m:Match) = 
        let dateTime = toDateTime m
        match m.Groups.[3].Value with 
                           | x when x.Contains("Guard") -> Start(dateTime=dateTime, guardId=guardId m.Groups.[3].Value) 
                           | x when x.Contains("falls asleep") -> Sleep(dateTime) 
                           | _ -> WakeUp(dateTime)
    toType matches

let getDate (s: string) = s.Substring(1, 16)
let schedules (y:string) xs = 
    match y with
    | s when s.Contains("Guard #") -> match xs with | x::xs -> [] :: (s::x) :: xs | [] -> [[s]]
    | s -> match xs with | x::xs -> (s::x) :: xs | [] -> [[s]] 

let toSleepEntries entries =
    let getGuardId x = match x with | Start(_, y) -> Some y | _ -> None
    let firstGuardId = List.choose getGuardId >> List.head
    let guardId = entries |> firstGuardId
    let makeSleepTime times x =
        match x with
        | (Start (_, _), Sleep ds) -> { SleepMinute=ds.Minute; WakeMinute=(-1<minute>); GuardId=guardId} :: times // New time
        | (Sleep _, WakeUp dw) -> match times with first::others -> { first with WakeMinute=dw.Minute; } :: others | _ -> times  // Finish time
        | (WakeUp _, Sleep ds) -> { SleepMinute=ds.Minute; WakeMinute=(-1<minute>); GuardId=guardId} :: times // New time
        | _ -> times
    entries |> List.pairwise |> List.fold makeSleepTime []


let sleepTotals =
    let guardId x = x.GuardId
    let minutes x = seq { for minute in x.SleepMinute..1<minute>..(x.WakeMinute-1<minute>) do yield minute } 
    let maxBy (mk, mv) k v = if mv > v then (mk, mv) else (k, v)
    let sumAndMax = Map.fold (fun (m, s) k v ->  (maxBy m k v, s + v)) ((0<minute>, 0), 0)
    let sumMinutes counts key = counts |> addOrUpdate 1 ((+) 1) key
    let countsByMinute = List.map minutes >> Seq.concat >> Seq.fold sumMinutes Map.empty >> sumAndMax
    let applyToSecond apply (x, y) = (x, apply y)
    List.filter (List.isEmpty >> not)
    >> List.map (List.map toEntry)
    >> List.map toSleepEntries
    >> List.concat 
    >> List.groupBy guardId 
    >> List.map (applyToSecond countsByMinute)
let sortedInput = input |> Array.sortBy getDate

let puzzle4Part1 = 
    Array.foldBack schedules sortedInput [] 
    |> sleepTotals
    |> List.sortByDescending (fun (_,(_, count)) -> count)
    |> List.head
    |> (fun (id, ((minute, _), _)) -> minute * id)

let puzzle4Part2 = 
    Array.foldBack schedules sortedInput [] 
    |> sleepTotals
    |> List.sortByDescending (fun (_,((_, sum), _)) -> sum)
    |> List.head
    |> (fun (id, ((minute, _), _)) -> minute * id) 

