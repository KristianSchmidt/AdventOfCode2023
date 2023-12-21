#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 21 |> Array.map (fun s -> s.ToCharArray())

let maxR, maxC = data.Length-1, data[0].Length-1

let isValid (r,c) = r >= 0 && r <= maxR && c >= 0 && c <= maxC

let getNeighbors (r,c) =
    [| (r+1,c); (r-1,c); (r,c+1); (r,c-1) |]
    |> Array.filter isValid
    |> Array.filter (fun (r,c) -> data[r % maxR][c % maxC] <> '#')

let start =
    let row = data |> Array.findIndex (fun arr -> arr |> Array.contains 'S')
    let col = data[row] |> Array.findIndex ((=)'S')
    row, col

let simulate start endStep =
    let rec f current step =
        if step = endStep then
            current
        else
            let next = current |> Array.collect getNeighbors |> Array.distinct
            f next (step + 1)

    f [|start|] 0

let ans1 = simulate start 64 |> Array.length

ans1

/// Part 2

let ans2 = data

ans2