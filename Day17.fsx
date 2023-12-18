#load "Helpers.fsx"

open System
open System.Collections.Generic

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data2 =
    Helpers.Web.getInput 17
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))

let data = """2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533""".Split('\n') |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))

let data =
    """2413432
3215453""".Split('\n') |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))


let maxX, maxY = data.Length-1, data[0].Length-1

let solve startPos (endX,endY) =
    let calcPotential (x',y') = abs (endY-y') + abs (endX-x')

    let s = new System.Diagnostics.Stopwatch()
    s.Start()
    
    let frontier = PriorityQueue<int*int,int>()
    frontier.Enqueue(startPos, 0)

    let mutable cameFrom : Map<int*int,int*int> = [startPos, (-1,-1)] |> Map.ofList
    let mutable costSoFar : Map<int*int,int> = [(startPos, 0)] |> Map.ofList
    let mutable result = -1

    let getNeighbors pos =
        let (x',y') = cameFrom[pos]
        let (x,y) = pos
        let regularNeighbors =
            [
                (x+1,y); (x-1,y); (x,y+1); (x,y-1)
            ]
            |> List.filter ((<>)(x',y')) // can't go back
            |> List.filter (fun (x,y) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY)
        match Map.tryFind (x',y') cameFrom with
        | Some (x'',y'') ->
            match Map.tryFind (x'',y'') cameFrom with
            | Some (x''',y''') ->
                let dir1 = (x-x',y-y')
                let dir2 = (x'-x'',y'-y'')
                let dir3 = (x''-x''',y''-y''')
                if (dir1 = dir2 && dir2 = dir3) then
                    //printfn "hit it %A. (%i,%i) -> (%i,%i) -> (%i,%i) -> (%i,%i)" dir1 x''' y''' x'' y'' x' y' x y
                    regularNeighbors
                    |> List.filter ((<>)(x+x-x', y+y-y'))
                else
                    regularNeighbors
            | None -> regularNeighbors
        | None -> regularNeighbors

    while (frontier.Count > 0) do
        let current = frontier.Dequeue()
        let (x,y) = current
        printfn "Processing %A" current
        let cost = costSoFar[current]
        
        if (x = endX && y = endY) then
            printfn "Cost: %i" cost
            result <- cost
        else
            let neighbors = getNeighbors current
            printfn "Neighbors of %A: %A" current neighbors
            for n in neighbors do
                let (x',y') = n
                let new_cost = cost + data[x'][y']
                if Map.containsKey n costSoFar then
                    printfn "CurrentCost %i NewCost %i - CostSoFar %i" cost new_cost costSoFar[n]
                if Map.containsKey n costSoFar |> not || new_cost < costSoFar[n] then
                    costSoFar <- costSoFar |> Map.add n new_cost
                    frontier.Enqueue(n, new_cost + calcPotential n)
                    printfn "CameFrom %A = %A" n current
                    cameFrom <- cameFrom |> Map.add n current
                
     
    //printfn "%A -> %A examined %i nodes" startPos (endX, endY) i
    cameFrom

let path = solve (0,0) (maxX,maxY)

path[1,]


// 733 too high

let ans1 = data

ans1

/// Part 2

let ans2 = data

ans2