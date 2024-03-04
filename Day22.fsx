#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let zmin (_,_,_,_,zmin,_) = zmin

let data =
    Helpers.Web.getInput 22
    |> Array.map (
        function
        | Helpers.Regex "(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)"
            [x1;y1;z1;x2;y2;z2] -> (int x1,int x2,int y1,int y2,int z1,int z2)
    )
    |> Array.sortBy zmin

let axisOverlap (a1min,a1max,a2min,a2max) =
    if (a2min > a1max || a1min > a2max) then
        false
    else
        //Some (max a1min a2min, min a1max a2max)
        true

let isOverlap ((xmin1, xmax1, ymin1, ymax1, zmin1, zmax1),(xmin2, xmax2, ymin2, ymax2, zmin2, zmax2)) =
    let xOverlap = axisOverlap (xmin1, xmax1, xmin2, xmax2)
    let yOverlap = axisOverlap (ymin1, ymax1, ymin2, ymax2)
    let zOverlap = axisOverlap (zmin1, zmax1, zmin2, zmax2)

    xOverlap && yOverlap && zOverlap

let mkNewPos (xmin,xmax,ymin,ymax,zmin,zmax) =
    (xmin,xmax,ymin,ymax,zmin-1,zmax-1)

let overlapsExcept (data : (int*int*int*int*int*int) array) newPos i' =
    let mutable overlaps = false
    for i in 0 .. data.Length - 1 do
        if i <> i' then
            if isOverlap (newPos, data[i]) then
                overlaps <- true
    overlaps

let findLowestPos (data : (int*int*int*int*int*int) array) pos i =
    let mutable fell = false
    let rec f p =
        let newPos = mkNewPos p
        match zmin p, overlapsExcept data newPos i with
        | 1, _ -> p
        | _, true -> p
        | _, false ->
            fell <- true
            f newPos

    f pos, fell

let fallDown (data : (int*int*int*int*int*int) array) =
    let mutable falls = Set.empty
    let mutable stillFalling = true
    while stillFalling do
        stillFalling <- false
        for i in 0 .. data.Length - 1 do
            if zmin data[i] >= 1 then
                let (lowestPos, fell) = findLowestPos data data[i] i
                if fell then
                    stillFalling <- true
                    falls <- Set.add i falls
                    printfn "Still falling. %A -> %A" data[i] lowestPos
                    data[i] <- lowestPos

    falls

fallDown data

let canBeDisintegrated i =
    let newData = Array.copy data |> Array.removeAt i
    printfn "%i" i
    [|0..newData.Length-1|]
    |> Array.forall (fun i' ->
        findLowestPos newData newData[i'] i' |> snd |> not)

let ans1 =
    [|0..data.Length-1|]
    |> Array.Parallel.filter canBeDisintegrated
    |> Array.length

ans1

/// Part 2

let mutable processed = 0
let bricksFalling i =
    let newData = Array.copy data |> Array.removeAt i
    System.Threading.Interlocked.Increment(ref processed) |> ignore
    printfn "Processed: %i" processed
    fallDown newData |> Set.count

let ans2 =
    [|0..data.Length-1|]
    |> Array.Parallel.map bricksFalling
    |> Array.sum

ans2