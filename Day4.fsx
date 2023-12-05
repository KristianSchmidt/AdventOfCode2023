#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 4

let matches =
    data
    |> Array.map (fun s -> s.Substring(10).Split(" | ") |> Array.map (fun s -> s.Split(" ") |> Array.filter (String.IsNullOrEmpty >> not) |> Array.map int) |> Array.map Set.ofArray)
    |> Array.map (fun [|winners; mine|] -> Set.intersect winners mine |> Set.count)

let ans1 =
    matches
    |> Array.sumBy (fun i -> if i = 0 then 0. else 2. ** (float (i-1)))
    |> int

ans1

/// Part 2

let counts = Array.init matches.Length (fun _ -> 1)

for i in 0..matches.Length-1 do
    let currMatch = matches[i]
    for j in 1..currMatch do
        counts[i+j] <- counts[i] + counts[i+j]

let ans2 = Array.sum counts

ans2