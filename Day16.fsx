#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let (maxR, maxC, data) =
    let arr = Helpers.Web.getInput 16 |> Array.map (fun s -> s.ToCharArray())
    
    arr.Length-1, arr[0].Length-1, arr |> array2D

let example = """.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|...."""

//let (maxR, maxC, data) =
//    let arr = example.Split('\n') |> Array.map (fun s -> s.ToCharArray())
//    
//    arr.Length-1, arr[0].Length-1, arr |> array2D

type Direction = Up | Down | Left | Right

let start = ((0,-1), Right)

let nextSpotBeam ((r,c), dir) =
    let (cR,cC) =
        match dir with
        | Up    -> (r-1,c)
        | Down  -> (r+1,c)
        | Right -> (r,c+1)
        | Left  -> (r,c-1)

    if cR < 0 || cR > maxR || cC < 0 || cC > maxC then
        None
    else
        Some ((cR,cC),dir)

let nextBeams ((r,c), dir) =
    let dirs =
        match data[r,c], dir with
        | '.', _      -> [|dir|]
        | '/', Up     -> [|Right|]
        | '/', Down   -> [|Left|]
        | '/', Left   -> [|Down|]
        | '/', Right  -> [|Up|]
        | '\\', Up    -> [|Left|]
        | '\\', Down  -> [|Right|]
        | '\\', Left  -> [|Up|]
        | '\\', Right -> [|Down|]
        | '|', Right
        | '|', Left   -> [|Up; Down|]
        | '|', _      -> [|dir|]
        | '-', Up
        | '-', Down   -> [|Left; Right|]
        | '-', _      -> [|dir|]

    dirs |> Array.map (fun d -> (r,c),d)

let simulateBeam start =
    let rec f seen beams =
        let newBeams =
            beams
            |> Array.choose nextSpotBeam
            |> Array.collect nextBeams

        //printfn "NewBeams %A" newBeams
        if Array.isEmpty newBeams then
            seen |> Set.toSeq |> Seq.map fst |> Seq.distinct |> Seq.length
        else
            let newSeen = Set.union seen (Set.ofArray newBeams)
            let isNotSeen = fun x -> Set.contains x seen |> not
            let beamsNotSeen = newBeams |> Array.filter isNotSeen
            f newSeen beamsNotSeen
        
    f Set.empty [|start|]

let ans1 = simulateBeam start

ans1

/// Part 2

let starts =
    seq {
        for r in 0 .. maxR do
            yield (r,-1),Right
            yield (r,maxC+1),Left

        for c in 0 .. maxC do
            yield (-1,c),Down
            yield (maxR+1,c),Up
    }

let ans2 =
    starts
    |> Seq.map simulateBeam
    |> Seq.max

ans2