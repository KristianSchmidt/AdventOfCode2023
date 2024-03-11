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

let mapToGrid (r,c) =
    let row =
        let rest = r % data.Length
        if (rest < 0) then data.Length + rest else rest
    let col =
        let rest = c % data.Length
        if (rest < 0) then data.Length + rest else rest

    (row, col)

let getNeighbors (r,c) =
    [| (r+1,c); (r-1,c); (r,c+1); (r,c-1) |]
    |> Array.filter (mapToGrid >> (fun (r,c) -> data.[r % maxR].[c % maxC] <> '#'))

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

/// Part 2

// 1: 1, 2, 4
// 2: 4, 6, 9
// 3: 9, 12, 16
// 4: 16, 20, 25
// 5: 25, 30, 36
// 6: 36, 42, 49

// t_2: 2, 4, 6, 8, 10
// t_3:    5, 7, 9, 11

// t1_n = t3_n-1
// t2_n = t2_n-1 + 2*n
// t3_n = t3_n-1 + (2*n + 1)

let [|t1Count; t2Count; t3Count|] =
    simulate start (65+131)
    |> Array.map mapToGrid
    |> Array.countBy id
    |> Array.countBy snd
    |> Array.sortBy fst
    |> Array.map (snd >> int64)

let calc (steps : int64) =
    let hops = (steps - 65L) / 131L
    
    hops*hops,
    hops * (hops + 1L),
    (hops + 1L) * (hops + 1L)

let steps = 26501365L
calc steps
let (t1, t2, t3) = calc steps

let ans2 = t1 * t1Count + t2 * t2Count + t3 * t3Count

ans2