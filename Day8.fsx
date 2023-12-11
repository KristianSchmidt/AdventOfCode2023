#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 8

let dir = data[0].ToCharArray()

let map =
    data[2..]
    |> Array.map (function Helpers.Regex "(\w\w\w) = \((\w\w\w), (\w\w\w)\)" [s;l;r] -> (s,Map [('L',l); ('R',r)] ))
    |> Map.ofArray

let findSteps endCondition source =
    let rec f s (i : int) =
        match s with
        | s' when (endCondition s') -> i
        | s' -> f map.[s'].[dir[i % dir.Length]] (i+1)
    f source 0

let ans1 = findSteps ((=)"ZZZ") "AAA"

ans1

/// Part 2

let sources =
    map
    |> Map.filter (fun k v -> k.EndsWith("A"))
    |> Map.keys
    |> Array.ofSeq

let ans2 =
    sources
    |> Array.map (findSteps (fun s -> s.EndsWith('Z')) >> int64)
    |> Array.reduce Helpers.lcm

ans2