#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 23 |> Array.map (fun s -> s.ToCharArray())

let data2 = """#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#""".Split('\n') |> Array.map (fun s -> s.ToCharArray())

let maxR,maxC = data.Length-1, data.[0].Length-1
let start = (0,1)
let endPoint = maxR,maxC-1

let neighbors (r,c) (seen : Set<int*int>) =
    match data.[r].[c] with
    | '.' -> [(r+1,c);(r-1,c);(r,c+1);(r,c-1)]
             |> Seq.filter (fun (x,y) -> x >= 0 && x <= maxR && y >= 0 && y <= maxC)
             |> Seq.filter (fun (x,y) -> data.[x].[y] <> '#')
             |> Seq.filter (fun p -> Set.contains p seen |> not)
    | '^' -> [(r-1,c)] |> Seq.filter (fun p -> Set.contains p seen |> not)
    | 'v' -> [(r+1,c)] |> Seq.filter (fun p -> Set.contains p seen |> not)
    | '<' -> [(r,c-1)] |> Seq.filter (fun p -> Set.contains p seen |> not)
    | '>' -> [(r,c+1)] |> Seq.filter (fun p -> Set.contains p seen |> not)

let solve start =
    let mutable longest = 0
    let rec f p seen =
        if (p = endPoint) then
            let pathLength = Set.count seen
            if pathLength > longest then
                printfn "New longest: %i" pathLength
                longest <- pathLength
        else
            let n = neighbors p seen
            for p' in n do
                f p' (Set.add p' seen)

    f start (Set.ofList [])

    longest

let ans1 = solve start

ans1

/// Part 2

let datap2 =
    data
    |> Array.map (fun arr -> arr |> Array.map (fun c -> if c = '#' then '#' else '.'))

let neighbors2 (r,c) (seen : Set<int*int>) =
    [(r+1,c);(r-1,c);(r,c+1);(r,c-1)]
    |> Seq.filter (fun (x,y) -> x >= 0 && x <= maxR && y >= 0 && y <= maxC)
    |> Seq.filter (fun (x,y) -> datap2.[x].[y] <> '#')
    |> Seq.filter (fun p -> Set.contains p seen |> not)
    |> List.ofSeq

let findNextDescision (r,c) seen =
    let rec f p seen' len =
        let n = neighbors2 p seen'
        match Seq.length n with
        | 1 -> let p' = Seq.head n
               f p' (Set.add p' seen') (len+1)
        | x when x > 1 -> Some (p, seen', len)
        | 0 when p = endPoint -> Some (p, seen', len)
        | 0 -> None

    f (r,c) seen 1

let vertices =
    seq {
        for r in 0 .. maxR do
            for c in 0 .. maxC do
                if datap2.[r].[c] <> '#' then
                    let n = neighbors2 (r,c) Set.empty |> Seq.length
                    if (n > 2 || (r,c) = start || (r,c) = endPoint) then
                        yield (r,c)
    } |> List.ofSeq

let edgesForPoint (p : int*int) =
    neighbors2 p Set.empty
    |> List.choose (fun p' ->
        match findNextDescision p' (Set.ofList [p;p']) with
        | Some (dest, _, len) -> Some ((p, dest), len)
        | None -> None)

let graph =
    vertices
    |> List.collect edgesForPoint
    |> List.groupBy (fst >> fst)
    |> List.map (fun (s,xs) -> s, xs |> List.map (fun ((s,d),l) -> d,l) |> Map.ofList)
    |> Map.ofList

let solve2' start =
    let mutable longest = 0
    let mutable sols = 0
    let q = System.Collections.Generic.PriorityQueue<(int*int)*Set<int*int>*int,int>()
    q.Enqueue((start, Set.ofList [start], 0), 0)

    while q.Count > 0 do
        let (p,seen,d) = q.Dequeue()
        let ns = graph[p] |> Map.filter (fun k v -> Set.contains k seen |> not)
        for (n,l) in Map.toSeq ns do
            let pathLength = d + l
            if n = endPoint then
                sols <- sols + 1
                if pathLength > longest then
                    printfn "New longest: %i. Total sols: %i" pathLength sols
                    longest <- pathLength
            else
                q.Enqueue((n, Set.add n seen, pathLength), 0)

    longest

let ans2 = solve2' start

ans2