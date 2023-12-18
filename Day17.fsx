#load "Helpers.fsx"

open System
open System.Collections.Generic

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    Helpers.Web.getInput 17
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))
    
let maxX, maxY = data.Length-1, data[0].Length-1

type Pos = int*int*string

let addToDir (dir : string) (c : string) =
    if dir.EndsWith(c) then
        dir + c
    else
        c

let getNeighbors (pos : Pos) =
    let (x,y,dir) = pos
    let r = (x,y+1, addToDir dir "R")
    let l = (x,y-1, addToDir dir "L")
    let u = (x-1,y, addToDir dir "U")
    let d = (x+1,y, addToDir dir "D")
    match dir with
    | "UUU" | "DDD" -> [l; r]
    | "RRR" | "LLL" -> [u; d]
    | s when s.EndsWith("U") -> [u;l;r]
    | s when s.EndsWith("D") -> [d;l;r]
    | s when s.EndsWith("L") -> [u;d;l]
    | s when s.EndsWith("R") -> [u;d;r]
    | _ -> [u;d;r;l]
    |> List.filter (fun (x,y,_) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY)

let solve startPos =
    let frontier = PriorityQueue<Pos,int>()
    frontier.Enqueue(startPos, 0)
    let mutable costSoFar : Map<Pos,int> = [(startPos,0)] |> Map.ofList

    while frontier.Count > 0 do
        let current = frontier.Dequeue()
        let (x,y,_) = current

        if x = maxX && y = maxY then
            ()
        else
            for n in getNeighbors current do
                let (x',y',_) = n
                let newCost = costSoFar[current] + data[x'][y']
                if costSoFar.ContainsKey(n) |> not || newCost < costSoFar[n] then
                    costSoFar <- Map.add n newCost costSoFar
                    frontier.Enqueue(n, newCost + abs (maxY-y') + abs (maxX-x'))

    costSoFar

let ans1 =
    solve (0,0,"")
    |> Map.toList
    |> List.filter (fun ((x,y,_),_) -> x = maxX && y = maxY)
    |> List.minBy snd
    |> snd

ans1

/// Part 2

let getNeighbors2 (pos : Pos) =
    let (x,y,dir) = pos
    let r = [4..10] |> List.map (fun i -> (x,y+i,"R"))
    let l = [4..10] |> List.map (fun i -> (x,y-i,"L"))
    let u = [4..10] |> List.map (fun i -> (x-i,y,"U"))
    let d = [4..10] |> List.map (fun i -> (x+i,y,"D"))
    match dir with
    | "U" -> List.collect id [d;l;r]
    | "D" -> List.collect id [u;l;r]
    | "L" -> List.collect id [u;d;r]
    | "R" -> List.collect id [u;d;l]
    | _ -> List.collect id [u;d;r;l]
    |> List.filter (fun (x,y,_) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY)

let getCost ((x,y,_) : Pos) ((x',y',_) : Pos) =
    let mutable sum = 0
    let minX,maxX = min x x', max x x'
    let minY,maxY = min y y', max y y'
    for i in minX .. maxX do
        for j in minY .. maxY do
            if ((x,y) <> (i,j)) then
                sum <- sum + data[i][j]

    sum

let solve2 startPos =
    let frontier = PriorityQueue<Pos,int>()
    frontier.Enqueue(startPos, 0)
    let mutable costSoFar : Map<Pos,int> = [(startPos,0)] |> Map.ofList

    while frontier.Count > 0 do
        let current = frontier.Dequeue()
        let (x,y,_) = current

        if x = maxX && y = maxY then
            ()
        else
            for n in getNeighbors2 current do
                let (x',y',_) = n
                let newCost = costSoFar[current] + getCost current n
                if costSoFar.ContainsKey(n) |> not || newCost < costSoFar[n] then
                    costSoFar <- Map.add n newCost costSoFar
                    frontier.Enqueue(n, newCost + abs (maxY-y') + abs (maxX-x'))

    costSoFar

let ans2 =
    solve2 (0,0,"")
    |> Map.toList
    |> List.filter (fun ((x,y,_),_) -> x = maxX && y = maxY)
    |> List.minBy snd
    |> snd

ans2