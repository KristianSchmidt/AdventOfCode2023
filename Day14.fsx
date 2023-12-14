#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let getData () =
    Helpers.Web.getInput 14
    |> Array.map (fun s -> s.ToCharArray())
    |> array2D

let moveNorth (arr : char array2d) =
    let mutable moves = 0
    for r in 1..arr.GetLength(0)-1 do // can't move any on first row as they're already the most north they can be
        for c in 0..arr.GetLength(1)-1 do
            if (arr[r,c] = 'O' && arr[r-1,c] = '.') then
                arr[r-1,c] <- 'O'
                arr[r,c] <- '.'
                moves <- moves + 1

    moves

let simulate mover data =
    let rec f () =
        let moves = mover data
        //printfn "moves: %i" moves
        if (moves > 0) then f () else data

    f ()

let score (data : char array2d) =
    seq {
        for r in 0 .. data.GetLength(0)-1 do
            for c in 0..data.GetLength(1)-1 do
                if (data[r,c] = 'O') then
                    yield (data.GetLength(0)-r)
    }
    |> Seq.sum

let ans1 = score <| simulate moveNorth (getData())

ans1

/// Part 2


let moveSouth (arr : char array2d) =
    let mutable moves = 0
    for r in arr.GetLength(0)-2 .. -1 .. 0 do // can't move any on last row as they're already the most south they can be
        for c in 0..arr.GetLength(1)-1 do
            if (arr[r,c] = 'O' && arr[r+1,c] = '.') then
                arr[r+1,c] <- 'O'
                arr[r,c] <- '.'
                moves <- moves + 1

    moves

let moveWest (arr : char array2d) =
    let mutable moves = 0
    for r in 0..arr.GetLength(0)-1 do // can't move any on last row as they're already the most south they can be
        for c in 1..arr.GetLength(1)-1 do
            if (arr[r,c] = 'O' && arr[r,c-1] = '.') then
                arr[r,c-1] <- 'O'
                arr[r,c] <- '.'
                moves <- moves + 1

    moves

let moveEast (arr : char array2d) =
    let mutable moves = 0
    for r in 0..arr.GetLength(0)-1 do // can't move any on last row as they're already the most south they can be
        for c in arr.GetLength(1)-2 .. -1 .. 0 do
            if (arr[r,c] = 'O' && arr[r,c+1] = '.') then
                arr[r,c+1] <- 'O'
                arr[r,c] <- '.'
                moves <- moves + 1

    moves

let doCycle (data : char array2d) =
    data
    |> simulate moveNorth
    |> simulate moveWest
    |> simulate moveSouth
    |> simulate moveEast

let example =
    """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....""".Split('\n') |> Array.map (fun s -> s.ToCharArray()) |> array2D

let toString (arr : char array2d) =
    seq {
        for r in 0..arr.GetLength(0)-1 do
            yield new String(arr[r,*])
    }
    |> (fun se -> String.Join("", se))

let simulate2 (data' : char array2d) =
    let states' = [| toString data',0 |] |> Map.ofArray
    
    let rec f states data i =
        let res = doCycle data
        let stateString = toString res
        match Map.tryFind stateString states with
        | Some i' -> i', i, res
        | None ->
            let newStates = states |> Map.add stateString i
            f newStates res (i+1)
        
    f states' data' 1

let (firstSeen, nextSeen, data) = simulate2 (getData ())
let cycle = nextSeen - firstSeen

let final = 1_000_000_000
let remaining = (final - firstSeen) % cycle

let ans2 =
    [| 1.. remaining |]
    |> Array.fold (fun s _ -> doCycle s) data
    |> score

ans2