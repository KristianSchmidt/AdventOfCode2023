#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 5

let seeds = data[0].Substring(7).Split(' ') |> Array.map int64

let parseMap (arr : string array) =
    arr
    |> Array.map (fun s -> s.Split(' ') |> Array.map int64 |> (fun [|second; first; length|] -> ((first, first+length-1L, second))))

let maps =
    String.Join('\n', data).Split("\n\n", StringSplitOptions.None)
    |> Array.tail
    |> Array.map (fun s -> s.Split('\n') |> Array.tail |> parseMap)

let findIndex (map : (int64 * int64 * int64) array) (start : int64) =
    match map |> Array.tryFind (fun (r1, r2, second) -> start >= r1 && start <= r2) with
    | Some (r1, r2, second) -> second + (start - r1)
    | None -> start

let findLocation seed =
    maps
    |> Array.fold (fun s map -> findIndex map s) seed

let ans1 = seeds |> Array.map findLocation |> Array.min

ans1

/// Part 2

#time "on"

let seeds2 = data[0].Substring(7).Split(' ') |> Array.map int64 |> Array.chunkBySize 2

let findMin [|start; range|] =
    let incr = range / 1_000L
    let diffs = [|start ..incr.. (start+range)|] |> Array.map findLocation |> Array.pairwise |> Array.mapi (fun i (a,b) -> i, b - a)
    let pois = diffs |> Array.filter (snd >> ((<>) incr)) |> Array.map (fst >> int64)
    let pointsToCheck = Array.collect id [|
        // Start
        [| start |]
        // All pois
        pois |> Array.collect (fun i -> [| start+i*incr .. start+(i+1L)*incr |])
        // Last range
        [| start + incr * 1000L .. start + range |]
    |]
    printfn "Points: %i" pointsToCheck.Length
    pointsToCheck
    |> Array.map findLocation
    |> Array.min

let ans2 = seeds2 |> Array.map findMin |> Array.min

ans2