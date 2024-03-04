#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 21 |> Array.map (fun s -> s.ToCharArray())
(*
let data = """...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........""".Split('\n') |> Array.map (fun s -> s.ToCharArray())
*)

let maxR, maxC = data.Length-1, data.[0].Length-1

let isValid (r,c) = r >= 0 && r <= maxR && c >= 0 && c <= maxC

let getNeighbors (r,c) =
    [| (r+1,c); (r-1,c); (r,c+1); (r,c-1) |]
    |> Array.filter isValid
    |> Array.filter (fun (r,c) -> data.[r % maxR].[c % maxC] <> '#')

let start =
    let row = data |> Array.findIndex (fun arr -> arr |> Array.contains 'S')
    let col = data.[row] |> Array.findIndex ((=)'S')
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

// Part 2

let getNeighbors2 (r,c) =
    [| (r+1,c); (r-1,c); (r,c+1); (r,c-1) |]
    |> Array.filter (fun (r,c) ->
        let r' = if (r % data.Length < 0) then data.Length + (r % data.Length) else r % data.Length
        let c' = if (c % data.Length < 0) then data.Length + (c % data.Length) else c % data.Length
        //printfn "r: %i r': %i c': %i" r r' c'
        data.[r'].[c'] <> '#'
    )

let neighbors = Helpers.memoize getNeighbors2

let simulate2 start endStep =
    let rec f (curr : Map<int*int,int64>) step =
        if step = endStep then
            curr
        else
            if step % 10_000 = 0 then
                printfn "Step: %i" step
            
            let newMap =
                curr
                |> Map.toArray
                |> Array.collect (fun (k,v) -> neighbors k |> Array.map (fun n -> n,v))
                |> Array.groupBy fst
                |> Array.map (fun (k,arr) -> k, arr |> Array.sumBy snd)
                |> Map.ofArray

            f newMap (step + 1)

    f (Map.ofList [start,1L]) 0

simulate2 start 1