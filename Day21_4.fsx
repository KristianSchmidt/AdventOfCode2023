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

let mapToGrid (r,c) =
    let row =
        let rest = r % data.Length
        if (rest < 0) then data.Length + rest else rest
    let col =
        let rest = c % data.Length
        if (rest < 0) then data.Length + rest else rest

    (row, col)

mapToGrid (-131,0)

let getNeighbors (r,c) =
    [| (r+1,c); (r-1,c); (r,c+1); (r,c-1) |]
    |> Array.filter (mapToGrid >> (fun (r,c) -> data.[r].[c] <> '#'))

let isUnreachable (r,c) =
    [| (r+1,c); (r-1,c); (r,c+1); (r,c-1) |]
    |> Array.map mapToGrid
    |> Array.forall (fun (r,c) -> data.[r].[c] = '#')

let unreachables =
    seq {
        for r in 0..130 do
            for c in 0..130 do 
                if (isUnreachable (r,c)) then
                    yield (r,c)
    }
    |> Set.ofSeq

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

let printMapped (map : Map<(int*int),int>) =
    for i in 0 .. 130 do
        for j in 0 .. 130 do
            match Map.containsKey (i,j) map with
            | true -> printf "%i" (map[(i,j)])
            | false ->
                printf "%c" data.[i].[j]
                
        printfn ""

#time "on"
let res = simulate start (65+131*6)

// 1, 2, 4
// 4, 6, 9
// 9, 12, 16
// 16, 20, 25
// 25, 30, 36
// 36, 42, 49

let mapped = 
    res
    |> Array.map mapToGrid
    |> Array.countBy id
    |> Map.ofArray


mapped |> Map.values |> Seq.distinct

printMapped mapped


let fullSquare =
    simulate start 131
    |> Array.filter (fun (r,c) -> r >= 0 && r <= 130 && c >= 0 && c <= 130)
    |> Array.length
    |> int64

let steps = 26501365L

let hops = (steps - 65L) / 131L
let hInWholes = 2L*hops+1L

hInWholes * fullSquare
let wholes =
    [|0L..hops-1L|]
    |> Array.sumBy (fun i -> i*2L+1L)
    |> (fun x -> x * 2L)

wholes * fullSquare + (simulate start 65 |> Array.length |> int64)


(*
for i in 0 .. steps do

Calculate row up:
rowidx = startR - i
colMin = startC - (steps - i)
colMax = startC + (steps - i)

*)

let calc i steps =
    let (startR,startC) = start
    let rowIdxUp = startR - i
    let rowIdxDown = startR + i
    let colMin = startC - (steps - i)
    let colMax = startC + (steps - i)
    (rowIdxUp, rowIdxDown), (colMin, colMax)
    
calc 1 65

let calcRow row colMin colMax steps =
    let res' =
        [|colMin .. colMax|]
        |> Seq.map (fun c -> mapToGrid (row,c))
        |> Seq.filter (fun (r, c) -> (r + c) % 2 = steps % 2 && data.[r].[c] <> '#' && not (Set.contains (r,c) unreachables))
    let res = res' |> Seq.length
    //if (row = 14) then
    //    printfn "Row %i = %i" row res
    //    printfn "%A" (List.ofSeq res')
    res

calcRow 1 65 65 64

let sumSteps steps =
    [0 .. steps]
    |> List.map (fun i -> calc i steps)
    |> List.map
        (fun ((rowUp,rowDown),(colMin,colMax)) ->
            if (rowUp <> rowDown) then
                calcRow rowUp colMin colMax steps + 
                calcRow rowDown colMin colMax steps
            else
                calcRow rowUp colMin colMax steps
         )
    |> List.sum

sumSteps 65
simulate start 65
|> Array.filter (fst >> ((=)14))
|> Array.map snd |> Array.sort

data.[14].[57]

let print2 s =
    let manual = simulate start s |> Array.length
    let auto = sumSteps s
    printfn "Step: %i - Manual: %i - Auto: %i" s manual auto

print2 65
print2 (65+131)

let print n =
    let (x,y) = start
    let res = simulate start n
    for i in x-n .. x+n do
        for j in y-n .. y+n do
            match Array.contains (i,j) res with
            | true -> printf "O"
            | false ->
                let (x',y') = mapToGrid (i,j)
                printf "%c" data.[x'].[y']
                
        printfn ""

// row 14 & 83 off
print 65

simulate start 65
|> Array.groupBy fst
|> Array.map (fun (k,a) -> k, Array.length a)
|> Array.iter (fun (k,l) -> printfn "Row %i = %i" k l)


ans1

// Part 2

simulate start 65




let simulate2 start steps =
    let mutable seenEven = Set.ofList [start]
    let mutable seenUneven = Set.empty
    let mutable frontier = Set.ofList [start]
    
    let rec f step =
        if step > steps then
            if steps % 2 = 0 then seenEven else seenUneven
        else
            printfn "Step %i. Frontier size: %i" step (Set.count frontier)
            let mutable newFrontier = Set.empty
            for (r,c) in frontier do
                for (r',c') in [(r-1,c)
                                (r+1,c)
                                (r,c-1)
                                (r,c+1)] do
                    let (r'',c'') = mapToGrid (r',c')
                    if data[r''][c''] <> '#' then
                        if step % 2 = 0 && Set.contains (r',c') seenUneven |> not then
                            newFrontier <- Set.add (r',c') newFrontier
                        else if step % 2 = 1 && Set.contains (r',c') seenEven |> not then
                            newFrontier <- Set.add (r',c') newFrontier
                        else
                            ()
                    else
                        ()
            frontier <- newFrontier
            if step % 2 = 1 then
                seenEven <- Set.union seenEven newFrontier
            else
                seenUneven <- Set.union seenUneven newFrontier
            f (step + 1)
    f 0


#time "on"
simulate2 start (65+131*2)
|> Set.toArray
|> Array.sortBy fst
|> Array.groupBy fst |> Array.map (fun a -> fst a, snd a |> Array.length)
|> Array.iter (printfn "%A")


simulate2 start 64 |> Set.count

let steps = 26501365

(steps - 65) / 131

